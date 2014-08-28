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
  (stacks-map [stacks] "returns a map of pc to stacks")
  (add [stacks carry]))

(defrecord Carried [stacks carry]
  Stacks
  (stacks-map [c]
    (reduce-kv (fn [m pc stacks] (assoc m pc (add stacks carry))) {} (stacks-map stacks)))
  (add [c carry']
    (Carried. stacks (combine carry carry'))))

(extend-protocol Stacks
  clojure.lang.APersistentMap
  (stacks-map [m] m)
  (add [m carry]
    (->Carried m carry))
  clojure.lang.Delay
  (stacks-map [d]
    (let [r @d]
      (if (delay? r)
        (recur r)
        (stacks-map r))))
  (add [d carry]
    (->Carried d carry)))

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
    (delay (merge-with merge-stacks (stacks-map a) (stacks-map b)))))

(defn plus [m pc tails]
  (assoc m pc (if-some [tails' (m pc)]
                (merge-stacks tails' tails)
                tails)))

(defn stepper
  [pgm]
  (letfn [(fail [_] false)
            (flow [m pos pc tails]
              (if (neg? pc)
                (plus m pc tails)
                (case (nth pgm pc)
                  :FORK (-> m (flow pos (+ pc 2) (add tails {:priority [0]})) (recur pos (nth pgm (inc pc)) (add tails {:priority [1]})))
                  :JUMP (recur m pos (nth pgm (inc pc)) tails)
                  :CALL (recur m pos (nth pgm (inc pc)) {(+ pc 2) (add tails {:events [[:push (inc pos)]]})})
                  :RET (reduce-kv #(flow %1 pos %2 %3) m (stacks-map (add tails {:events [[:pop (inc pos) (nth pgm (inc pc))]]})))
                  :PRED (plus m pc tails))))
            (step [stacks pos c m]
              (reduce-kv (fn [m pc tails]
                           (if ((nth pgm (inc pc) fail) c)
                             (flow m pos (+ pc 2) tails)
                             (plus m pc (add tails {:error 1 :events [[:skip pos]]}))))
                m (stacks-map stacks)))]
      (let [init-stacks (flow {} -1 0 (plus {} -1 {}))]
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
