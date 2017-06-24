(ns day1.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn big
  "Returns true if and only if str is longer than n characters"
  [str n]
  (< n (count str)))

(defn collection-type
  "Returns a keyword corresponding to the type of col"
  [col]
  (let [col-class (class col)]
    (cond
      (= col-class clojure.lang.PersistentList) :list
      (= col-class clojure.lang.PersistentVector) :vector
      (= col-class clojure.lang.PersistentArrayMap) :map )))