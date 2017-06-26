(ns sleeping-barber.core)

;;; Solution to the sleeping barber problem. Because we have 4 chairs to keep
;;; in sync, and we don't want a barber to lose haircuts, we use STM as our
;;; basic concurrency primitive. This ensures no person can be in two seats
;;; at once, and two people can't move the a single seat at the same time.


(def run (atom true))

;; Define a ref for people who got their hair cut
(def got-hair-cut (ref 0))

;; Make an alias for the barbers chair being empty
(def empty true)

;; Barbers chair
(def barbers-chair (ref empty))

;; People waiting
(def waiting (ref 0))
(def max-waiting 3)

(defn continuously-cut-hair
  []
  (loop []
      (dosync
       (if (and @barbers-chair (> @waiting 0))
         (do
           (alter waiting dec)
           (alter barbers-chair not))))
      (if (not @barbers-chair)
        (do
          (Thread/sleep 20)
          (dosync
           (alter got-hair-cut inc)
           (alter barbers-chair not))))
      (if @run
        (recur))))


(defn until-next-customer
  "Provides a random interval until the next customer arrives"
  []
  (+ 10 (rand-int 20)))

(defn join-or-leave
  [queue]
  (if (< queue max-waiting)
    (inc queue)
    queue))

(defn generate-customers
  []
  (loop []
    (Thread/sleep (until-next-customer))
    (dosync
     (alter waiting join-or-leave))
    (if @run
      (recur))))


(defn cut-hair
  []
  (let [barber (future (continuously-cut-hair)),
        customers (future (generate-customers))]
    (Thread/sleep 10000)
    ;; Cancelling futures doesn't seem robust in tight loops,
    ;; so we use a killswitch atom
    (let [done @got-hair-cut]
      (swap! run not)
      done)))
