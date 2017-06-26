(ns day3.core)

;; A test account
(def account (ref [10 20 30 40]))

(defn increase-at
  "Return a function which increases entry n by v in the passed vector"
  [n v]
  (fn [a]
    (assoc a n (+ (a n) v))))

(defn decrease-at
  "Return a function which decreases entry n by v in the passed vector"
  [n v]
  (increase-at n (- v)))

(defn credit
  "Atomically increase account n by v in a"
  [n v a]
  (dosync
   (alter a (increase-at n v))))

(defn debit
  "Atomically decrease account n by v in a"
  [n v a]
  (credit n (- v) a))

(defn transfer
  "Atomically transfer v from n to m in a"
  [n m v a]
  (dosync
   (alter a (increase-at m v))
   (alter a (decrease-at n v))))
  
