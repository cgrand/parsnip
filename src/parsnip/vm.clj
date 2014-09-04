(ns parsnip.vm)

(defn has-priority? [a b]
  (if-let [[a & as] (seq a)]
    (if-let [[b & bs] (seq b)]
      (if (= a b)
        (recur as bs)
        (< a b))
      false)
    true))

(defn combine [a b]
  {:error (+ (:error a 0) (:error b 0))
   :events (concat (:events a nil) (:events b nil))
   :priority (concat (:priority a nil) (:priority b nil))})

(defprotocol Stacks
  (stacks-map [stacks] "Returns a map of pc to stacks.")
  (add [stacks carry])
  (fathomings [stacks] "Returns a set of stacks fathomings."))

(defrecord Carried [stacks carry]
  Stacks
  (stacks-map [c]
    (reduce-kv (fn [m pc stacks] (assoc m pc (add stacks carry))) {} (stacks-map stacks)))
  (add [c carry']
    (Carried. stacks (combine carry carry')))
  (fathomings [c] (fathomings stacks)))

(defrecord Delayed [d fathomings]
  Stacks
  (stacks-map [delayed]
    (loop [r @d]
      (if (delay? r)
        (recur @r)
        (stacks-map r))))
  (add [delayed carry]
    (->Carried delayed carry))
  (fathomings [delayed] fathomings))

(defrecord FSMStacks [pcs transitions path]
  Stacks
  (stacks-map [fsm-stacks]
    (reduce (fn [m pc] (assoc m pc (FSMStacks. (transitions pc #{}) transitions (conj path pc)))) {} pcs))
  (add [fsm-stacks carry]
    (->Carried fsm-stacks carry))
  (fathomings [m]
    #{path}))

(defrecord ConsStacks [pc stacks]
  Stacks
  (stacks-map [cons-stacks] {pc stacks})
  (add [cons-stacks carry]
    (->Carried cons-stacks carry))
  (fathomings [m]
    (fathomings stacks)))

(extend-protocol Stacks
  clojure.lang.APersistentMap
  (stacks-map [m] m)
  (add [m carry]
    (->Carried m carry))
  (fathomings [m]
    (reduce #(into %1 (fathomings %2)) #{} m)))

(def bottom (->FSMStacks #{-2} {} []))

(defn merge-stacks [a b]
  (cond
    (= a b) a
    (and (instance? Carried a) (instance? Carried b) (= (:stacks a) (:stacks b)))
    (cond
      (< (:error (:carry a)) (:error (:carry b))) a
      (> (:error (:carry a)) (:error (:carry b))) b
      (has-priority? (:priority (:carry a)) (:priority (:carry b))) a
      :else b)
    :else
    (->Delayed (delay (merge-with merge-stacks (stacks-map a) (stacks-map b)))
      (into (fathomings a) (fathomings b)))))

(defn plus [m pc tails]
  (assoc m pc (if-some [tails' (m pc)]
                (merge-stacks tails' tails)
                tails)))

(defn push [stacks pc] (->ConsStacks pc stacks))

(defn stepper
  [pgm]
  (letfn [(flow [m pos pc tails has-c c spc stails]
            (if (neg? pc)
              (if has-c
                (plus m spc (add stails {:error 1 :events [[:skip pos]]}))
                (plus m pc tails))
              (case (nth pgm pc)
                :FORK (-> m (flow pos (+ pc 2) (add tails {:priority [0]}) has-c c spc stails)
                        (recur pos (nth pgm (inc pc)) (add tails {:priority [1]}) has-c c spc stails))
                :JUMP (recur m pos (nth pgm (inc pc)) tails has-c c spc stails)
                :CALL (recur m pos (nth pgm (inc pc)) (push (add tails {:events [[:push pos]]}) (+ pc 2)) has-c c spc stails)
                :RET (reduce-kv #(flow %1 pos %2 %3 has-c c spc stails) m (stacks-map (add tails {:events [[:pop pos (nth pgm (inc pc))]]})))
                :PEEK (if has-c
                        (if ((nth pgm (inc pc)) c)
                          (recur m pos (+ pc 2) tails true c spc stails)
                          (plus m spc (add stails {:error 1 :events [[:skip pos]]})))
                        (plus m pc tails))
                :PRED (if has-c
                        (if ((nth pgm (inc pc)) c)
                          (recur m (inc pos) (+ pc 2) tails false c spc stails)
                          (plus m spc (add stails {:error 1 :events [[:skip pos]]})))
                        (plus m pc tails)))))
            (step [stacks pos c m]
              (reduce-kv (fn [m pc tails]
                           (flow m pos pc tails true c pc tails))
                m (stacks-map stacks)))]
      (let [init-stacks (flow {} 0 0 bottom false nil 0 bottom)]
        (fn 
          ([] init-stacks)
          ([stacks pos c]
            (step stacks pos c {}))))))

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

(defn all-stacks [stacks]
  (let [m (stacks-map stacks)]
    (if (= m {}) [[() (:carry stacks)]]
      (for [[pc stacks] m
            [stack carry] (all-stacks stacks)]
        [(cons pc stack) carry]))))