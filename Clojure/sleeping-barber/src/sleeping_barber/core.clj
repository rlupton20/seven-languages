(ns sleeping-barber.core)

;;; Solution to the sleeping barber problem. Because we have 4 chairs to keep
;;; in sync, and we don't want a barber to lose haircuts, we use STM as our
;;; basic concurrency primitive. This ensures no person can be in two seats
;;; at once, and two people can't move the a single seat at the same time.

;;; Run with (cut-hair)


(defn new-barber-shop
  "Create the state for a new barber shop, and run
   of the experiment"
  []
  (let [ run (atom true)
         barbers-chair (ref true)
         got-hair-cut (ref 0)
         waiting (ref 0) ]
    { 'run run
     , 'barbers-chair barbers-chair
     , 'got-hair-cut got-hair-cut
     , 'waiting waiting }
    ))


(defn is-empty
  "Predicate for barbers chair being empty"
  [barbers-chair]
  barbers-chair)


(defn toggle
  "Function to change state of barbers chair, from empty to full, or
   full to empty"
  [barbers-chair]
  (not barbers-chair))


(defn continuously-cut-hair
  [shop]
  (let [ run (shop 'run)
         barbers-chair (shop 'barbers-chair)
         got-hair-cut (shop 'got-hair-cut)
         waiting (shop 'waiting) ]
    (loop []
      (dosync
       (if (and (is-empty @barbers-chair) (> @waiting 0))
         (do
           (alter waiting dec)
           (alter barbers-chair toggle))))
      (if (not (is-empty @barbers-chair))
        (do
          (Thread/sleep 20)
          (dosync
           (alter got-hair-cut inc)
           (alter barbers-chair toggle))))
      (if @run
        (recur)))))


(defn until-next-customer
  "Provides a random interval until the next customer arrives"
  []
  (+ 10 (rand-int 20)))


(defn join-or-leave
  "When a customer arrives, should we join the waiting queue or leave"
  [queue]
  (let [max-waiting 3]
    (if (< queue max-waiting)
      (inc queue)
      queue)))


(defn generate-customers
  [shop]
  (let [ run (shop 'run)
         waiting (shop 'waiting) ]
    (loop []
      (Thread/sleep (until-next-customer))
      (dosync
       (alter waiting join-or-leave))
      (if @run
        (recur)))))


(defn cut-hair
  []
  (let [ shop (new-barber-shop)
         run (shop 'run)
         got-hair-cut (shop 'got-hair-cut) ]
    (let [barber (future (continuously-cut-hair shop)),
          customers (future (generate-customers shop))]
      (Thread/sleep 10000)
      ;; Cancelling futures doesn't seem robust in tight loops,
      ;; so we use a killswitch atom
      (let [done @got-hair-cut]
        (swap! run not)
        done))))
