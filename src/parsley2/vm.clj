(ns parsley2.core)

(defn conj-stacks
  ([] {})
  ([stacks] stacks)
  ([stacks stacks'] (reduce-kv conj-stacks stacks stacks'))
  ([stacks head tails]
    (assoc stacks head 
      (if-some [tails' (stacks head)]
        (conj-stacks tails' tails)
        tails))))

(defn stepper
  ([pgm] (stepper pgm conj-stacks))
  ([pgm conj-stacks]
    (letfn [(step1 [pc tails c acc-stacks]
              (case (nth pgm pc)
                :FORK (->> acc-stacks (step1 (+ pc 2) tails c) (recur (nth pgm (inc pc)) tails c)) 
                :JUMP (recur (nth pgm (inc pc)) tails c acc-stacks)
                :CALL (recur (nth pgm (inc pc)) (push tails (+ pc 2)) c acc-stacks)
                :RET (step tails c acc-stacks)
                :PRED (if ((nth pgm (inc pc)) c)
                        (conj-stacks acc-stacks (+ pc 2) tails)
                        acc-stacks)))
            (step [stacks c acc-stacks]
              (reduce-kv (fn [acc-stacks pc tails]
                           (step1 pc tails c acc-stacks)) acc-stacks stacks))]
      #(conj-stacks (step %1 %2 (conj-stacks))))))

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