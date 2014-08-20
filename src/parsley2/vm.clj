(ns parsley2.core)

(defn merge-stacks [a b]
  (merge-with merge-stacks a b))

(defn push [tails pc] {pc tails})

(defn stepper [pgm]
  (letfn [(step1 [pc tails c]
            (case (nth pgm pc)
              :FORK (merge-stacks (step1 (+ pc 2) tails c) (step1 (nth pgm (inc pc)) tails c)) 
              :JUMP (recur (nth pgm (inc pc)) tails c)
              :CALL (recur (nth pgm (inc pc)) (push tails (+ pc 2)) c)
              :RET (step tails c)
              :PRED (if ((nth pgm (inc pc)) c)
                      (push tails (+ pc 2))
                      {})))
          (step [stacks c]
            (reduce-kv (fn [stacks pc tails]
                         (merge-stacks stacks (step1 pc tails c))) {} stacks))]
    step))

(def pgm
  [:FORK 5
   :PRED #(= % \x)
   :RET
   :PRED #(= % \()
   :FORK 13
   :CALL 0
   :JUMP 7
   :PRED #(= % \))
   :RET])

(def st (stepper pgm))