(ns baby.pat.mod-repl.utils)

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

;; Copied from clojure.main to satisfy portability requirements.
(defn skip-whitespace
  [s]
  (loop [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     (= c (int \;)) (do (.readLine s) :line-start)
     (or (Character/isWhitespace (char c)) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

;; Copied from clojure.main to satisfy portability requirements.
(defn skip-if-eol
  [s]
  (let [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     :else (do (.unread s c) :body))))

(defn extract-value-leave-context [input]
  (let [read-eval *read-eval*
        value (binding [*read-eval* read-eval] (eval input))
        _ (set! *3 *2)
        _ (set! *2 *1)
        _ (set! *1 value)]
    value))
