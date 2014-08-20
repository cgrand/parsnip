(ns parsley2.core)

(def stacks-as-maps-no-error
  {:zero {}
   :plus (letfn [(merge-stacks [stacks stacks'] (reduce-kv conj-stacks stacks stacks'))
                 (conj-stacks [stacks head tails]
                   (assoc stacks head 
                     (if-some [tails' (stacks head)]
                       (merge-stacks tails' tails)
                       tails)))]
           conj-stacks)
   :plus-failed (fn [stacks head tails]
                  stacks)
   :reduce-stacks reduce-kv})

(def stacks-as-maps-with-error
  (letfn [(carry [stacks n]
            (assoc stacks :error (+ n (:error stacks 0))))
          (reduce-stacks [f acc stacks]
            (let [error (:error stacks 0)]
              (reduce-kv (fn [acc head tails] (f acc head (carry tails error)))
                acc (dissoc stacks :error))))
          (merge-stacks [stacks stacks']
            (let [error (:error stacks 0)
                  error' (:error stacks' 0)
                  d (- error' error)]
              (cond 
                (= (dissoc stacks :error) (dissoc stacks' :error))
                (if (neg? d) stacks' stacks)
                (neg? d)
                (reduce-stacks plus stacks' (assoc stacks :error (- d)))
                :else
                (reduce-stacks plus stacks (assoc stacks' :error d)))))
          (plus [stacks head tails]
            (assoc stacks head 
              (if-some [tails' (stacks head)]
                (merge-stacks tails' tails)
                tails)))]
    {:zero {}
     :plus plus
     :plus-failed (fn [stacks head tails]
                    (plus stacks head (carry tails 1)))
     :reduce-stacks reduce-stacks}))

(defn stepper
  ([pgm] (stepper pgm stacks-as-maps-no-error))
  ([pgm {:keys [zero plus plus-failed reduce-stacks]}]
    (letfn [(fail [_] false)
            (flow [acc-stacks pc tails]
              (if (neg? pc)
                (plus acc-stacks pc tails)
                (case (nth pgm pc)
                  :FORK (-> acc-stacks (flow (+ pc 2) tails) (recur (nth pgm (inc pc)) tails)) 
                  :JUMP (recur acc-stacks (nth pgm (inc pc)) tails)
                  :CALL (recur acc-stacks (nth pgm (inc pc)) (push tails (+ pc 2)))
                  :RET (reduce-stacks flow acc-stacks tails)
                  :PRED (plus acc-stacks pc tails))))
            (step [stacks c acc-stacks]
              (reduce-stacks (fn [acc-stacks pc tails]
                               (if ((nth pgm (inc pc) fail) c)
                                 (flow acc-stacks (+ pc 2) tails)
                                 (plus-failed acc-stacks pc tails)))
                acc-stacks stacks))]
      (let [init-stacks (flow zero 0 (plus zero -1 zero))]
        (fn 
          ([] init-stacks)
          ([stacks c]
            (step stacks c zero)))))))

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
(def ste (stepper pgm stacks-as-maps-with-error))