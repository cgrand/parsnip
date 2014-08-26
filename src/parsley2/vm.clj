(ns parsley2.vm)

(def stacks-ops
  (letfn [(carry [stacks error events]
            (assoc stacks :error (+ error (:error stacks 0)) :events (concat (:events stacks) events)))
          (reduce-stacks [f init stacks]
            (let [error (:error stacks 0)
                  events (:events stacks)]
              (reduce-kv (fn [acc head tails] (f acc head (carry tails error events)))
                init (dissoc stacks :error :events))))
          (propagate-carry [stacks]
            (let [value (:error stacks 0)
                  events (:events stacks)]
              (if (and (= 0 value) (empty? events))
                stacks
                (reduce-stacks assoc {} stacks))))
          (merge-stacks [stacks stacks']
            (if (= (dissoc stacks :error :events) (dissoc stacks' :error :events))
              (if (< (:error stacks' 0) (:error stacks 0)) stacks' stacks)
              (reduce-stacks unsafe-plus (propagate-carry stacks) stacks')))
          (plus [stacks head tails]
            (unsafe-plus (propagate-carry stacks) head tails))
          (unsafe-plus [stacks0 head tails]
            (assoc stacks0 head 
              (if-some [tails' (stacks0 head)]
                (merge-stacks tails' tails)
                tails)))]
    {:plus plus
     :reduce-stacks reduce-stacks
     :push (fn [tails pc pos] (plus {} pc (carry tails 0 [[:push pos]])))
     :pop (fn [tails x pos] (carry tails 0 [[:pop pos x]]))
     :skip (fn [tails pos] (carry tails 1 [[:skip pos]]))}))

(defn stepper
  [pgm]
  (let [{:keys [plus reduce-stacks push pop skip]} stacks-ops]
    (letfn [(fail [_] false)
            (flow [acc-stacks pos pc tails]
              (if (neg? pc)
                (plus acc-stacks pc tails)
                (case (nth pgm pc)
                  :FORK (-> acc-stacks (flow pos (+ pc 2) tails) (recur pos (nth pgm (inc pc)) tails)) 
                  :JUMP (recur acc-stacks pos (nth pgm (inc pc)) tails)
                  :CALL (recur acc-stacks pos (nth pgm (inc pc)) (push tails (+ pc 2) pos))
                  :RET (reduce-stacks #(flow %1 pos %2 %3) acc-stacks (pop tails (nth pgm (inc pc)) pos))
                  :PRED (plus acc-stacks pc tails))))
            (step [stacks pos c acc-stacks]
              (reduce-stacks (fn [acc-stacks pc tails]
                               (if ((nth pgm (inc pc) fail) c)
                                 (flow acc-stacks pos (+ pc 2) tails)
                                 (plus acc-stacks pc (skip tails pos))))
                acc-stacks stacks))]
      (let [init-stacks (flow {} -1 0 (plus {} -1 {}))]
        (fn 
          ([] init-stacks)
          ([stacks pos c]
            (step stacks pos c {})))))))

(defn link [pgm]
  (let [labels (reduce (fn [labels pc]
                         (let [label (nth pgm (inc pc))
                               pc (- pc (* 2 (count labels)))]
                           (when-some [pc' (labels label)]
                             (throw (ex-info "Label used twice." {:label label :pcs [pc' pc]})))
                           (assoc labels label pc)))
                 {} 
                 (filter #(= :LABEL (nth pgm %)) (range 0 (count pgm) 2)))]
    (vec (mapcat (fn [[op x]]
                   (case op
                     (:CALL :JUMP :FORK) [op (or (labels x) (throw (ex-info "Label not found." {:label (labels x)})))]
                     :LABEL nil
                     [op x])) (partition 2 pgm)))))

(def pgm
  (link
    [:LABEL :E
     :FORK ["(" :E* ")"]
     :PRED #(= % \x)
     :RET nil
     :LABEL ["(" :E* ")"]
     :PRED #(= % \()
     :LABEL :E*
     :FORK :end-of-E*
     :CALL :E
     :JUMP :E*
     :LABEL :end-of-E*
     :PRED #(= % \))
     :LABEL :end
     :RET nil]))

(def pgm2
  (link
    [:LABEL :E
     :FORK [:X "+" :E]
     :LABEL :X
     :FORK ["(" :E ")"]
     :PRED #(= % \x)
     :RET "X"
     :LABEL ["(" :E ")"]
     :PRED #(= % \()
     :CALL :E
     :PRED #(= % \))
     :RET "(E)"
     :LABEL [:X "+" :E]
     :CALL :X
     :PRED #(= % \+)
     :CALL :E
     :RET "X+E"]))

(defn all-stacks [stacks]
  (let [v (:value stacks 0)]
    (if (= {} (dissoc stacks :value))
      [(list v)]
      (for [[pc stacks] stacks
            :when (number? pc)
            [err & stack] (all-stacks stacks)]
        (list* (+ v err) pc stack)))))

; knowing i'm at a given pc, what where the previous

(defn- pred-pcs [pgm] (filter #(= (nth pgm %) :PRED) (range 0 (count pgm) 2)))

(defn- call-targets
  "Returs the set of pcs that appear as the argument of a call."
  [pgm]
  (set (keep #(when (= (nth pgm %) :CALL) (nth pgm (inc %))) (range 0 (count pgm) 2))))

(defn- call-sites [pgm]
  (reduce (fn [call-sites pc]
            (if (= (nth pgm pc) :CALL)
              (let [target (nth pgm (inc pc))]
                (assoc call-sites target (conj (call-sites target #{}) pc)))
              call-sites)) {} (range 0 (count pgm) 2)))

(defn- successors [pgm pc]
  (let [op (nth pgm pc)]
    (case op
      :RET nil
      :FORK [(nth pgm (inc pc)) (+ pc 2)]
      :JUMP [(nth pgm (inc pc))]
      [(+ pc 2)])))

(defn- rets
  "Returns a map of pcs to set of reachable ret pcs,"
  [pgm]
  (letfn [(rets [known-rets pc]
            (if (contains? known-rets pc)
              known-rets
              (let [op (nth pgm pc)]
                (assoc known-rets pc
                  (if (= :RET op)
                    #{pc}
                    (let [succs (successors pgm pc)
                          known-rets (dissoc (reduce rets (assoc known-rets pc #{}) succs) pc)]
                      (reduce #(into %1 (known-rets %2)) #{} succs)))))))]
    (reduce rets {} (range 0 (count pgm) 2))))

(defn- return-targets
  "Returns a map from ret pcs to sets of return targets."
  [pgm]
  (let [call-sites (call-sites pgm)
        rets (rets pgm)]
    (reduce (fn [return-targets [ret caller+2]]
              (assoc return-targets ret (conj (return-targets ret #{}) caller+2)))
      {} (for [[callee callers] call-sites
               ret (rets callee)
               caller callers]
           [ret (+ caller 2)]))))

#_(let [step (stepper pgm2)]
    (reduce-kv  step (step) (vec "(x+x")))