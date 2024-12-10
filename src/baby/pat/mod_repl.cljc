(ns baby.pat.mod-repl
  (:require [baby.pat.mod-repl.utils :as util]
            [clojure.java.io]
            [clojure.pprint])
  (:import clojure.lang.LineNumberingPushbackReader))

;; API
(defmulti mod-enter  :enter-with)
(defmulti mod-repl   :repl-with)
(defmulti mod-read   :read-with)
(defmulti mod-eval   :eval-with)
(defmulti mod-print  :print-with)
(defmulti mod-loop   :loop-with)
(defmulti mod-exit   :exit-with)

;;; ### Default Print
(defmethod mod-print :default [{:keys [value] :as config}]
  (doall [(print "=> ")
          (clojure.pprint/pprint value)
          (println)]))

;;; ### Default Eval
(defmethod mod-eval :default
  [config input]
  (util/extract-value-leave-context input))

;;; ### Default Read
(defmethod mod-read :default
  [{:keys [request-prompt request-exit] :as config}]
  (assoc config
         :input (util/with-read-known
                  (or ({:line-start request-prompt :stream-end request-exit}
                       (util/skip-whitespace *in*))
                      (let [*input (read {:read-cond :allow} *in*)]
                        (util/skip-if-eol *in*)
                        (case *input
                          :repl/quit request-exit
                          *input))))))

(defn read-eval-print
  [{:keys [request-prompt request-exit] :as config}]
  (let [read-results (mod-read config)
        {:keys [input] :as eval-results} (mod-eval read-results)]
    (if (#{request-prompt request-exit} input)
      input
      (mod-print eval-results))))

;;; ### Default Loop
#?(:bb (def ^:dynamic *source-path* nil))
(defmethod mod-loop :default
  [{:keys [exit-with] :as config}]
  ;; Sets up a classloader on JVM. And an atom that holds the context.
  (let [cl #?(:bb nil
              :clj (.getContextClassLoader (Thread/currentThread)))
        _ #?(:bb nil
             :clj (.setContextClassLoader
                   (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl)))
        value-holder (atom config)]
    (try
      (loop []
        (let [{:keys [result request-exit] :as config} (read-eval-print config)
              _ (reset! value-holder config)]
          (when-not (identical? result request-exit)
            (recur))))
      (finally
        (when (not (nil? exit-with))
          (mod-exit @value-holder))))))

;; # mod-repl
(defmethod mod-repl :default
  [config]
  (let [{:keys [nmsp script]} config        
        ns (symbol nmsp)]
    (binding [*ns* *ns*]
      (in-ns ns)
      (clojure.core/use 'clojure.core)
      (with-open [rdr (LineNumberingPushbackReader. (clojure.java.io/reader script))]
          (binding [*source-path* script *in* rdr]
            (mod-loop config))))))

(defmethod mod-enter :default
  [config] config)

(def default-config {:enter-with :default
                     :repl-with :default
                     :read-with :default
                     :eval-with :default
                     :print-with :default
                     :loop-with :default
                     :exit-with :default})

(defn play [config]
  (if (vector? config)
    (map play config)
    (let [config (mod-enter (assoc config :request-prompt (Object.) :request-exit (Object.)))]
      (mod-repl config))))
