(ns parsnip.asm)

(defn- map-targets-drop-labels [f pgm]
  (vec (mapcat (fn [[op x]]
                   (case op
                     (:CALL :JUMP :FORK) [op (f x)]
                     :LABEL nil
                     [op x])) (partition 2 pgm))))

(defn link [pgm]
  (let [labels (reduce (fn [labels pc]
                         (let [label (nth pgm (inc pc))
                               pc (- pc (* 2 (count labels)))]
                           (when-some [pc' (labels label)]
                             (throw (ex-info "Label used twice." {:label label :pcs [pc' pc]})))
                           (assoc labels label pc)))
                 {} 
                 (filter #(= :LABEL (nth pgm %)) (range 0 (count pgm) 2)))]
    (map-targets-drop-labels #(or (labels %) (throw (ex-info "Label not found." {:label (labels %)}))) pgm)))

(defn unlink [pgm]
  (let [labels (into (sorted-map) (keep (fn [[op arg]] (case op (:FORK :JUMP :CALL) [arg (gensym :label_)] nil)) (partition 2 pgm)))
        slice (fn [from to]
                (map-targets-drop-labels labels (subvec pgm from to)))]
    (reduce (fn [unlinked-pgm [[from label] [to]]]
              (-> unlinked-pgm
                (conj :LABEL label)
                (into (slice from to))))
      (slice 0 (first (keys labels)))
      (partition 2 1 [[(count pgm)]] labels))))