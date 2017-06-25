(ns day2.core)


(defmacro unless
  ([test body]
   (list 'if (list 'not test) body))
  ([test body otherwise]
   (list 'if (list 'not test) body otherwise)))

(defmacro unless-2
  [test body & [otherwise]]
  (list 'if (list 'not test) body
        (if (nil? otherwise) nil otherwise)))

(defmacro unless-3
  "Use with :otherwise key before else clause e.g.
   (unless-3 true 0 :otherwise 1)"
  [test body & {:keys [otherwise] :or {otherwise nil}}]
  (list 'if (list 'not test) body otherwise))


;; We make up a protocol and implement a record for it
;; Our protocol describes something that can store state
(defprotocol State
  (put-state [c,s])
  (get-state [c]))

(defrecord Value [value]
  State
  (put-state [_ s]
    (Value. s))
  (get-state [_]
    value))

(def test-state (Value. 0))
