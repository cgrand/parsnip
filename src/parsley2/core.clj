(ns parsley2.core
  (:refer-clojure :exclude [+ *]))

(defprotocol Asmable
  (asm [x]))

(defrecord InlineAsm [ops]
  Asmable
  (asm [_] ops))

(extend-protocol Asmable
  clojure.lang.APersistentVector
  (asm [x] (mapcat asm x))
  clojure.lang.AMapEntry
  (asm [[k v]] 
    (let [[k r] (if-let [[_ pre] (re-matches #"(.*)-" (name k))]
                  [(keyword (namespace k) pre) nil]
                  [k k])]
      (concat [:LABEL k] (asm v) [:RET r])))
  clojure.lang.APersistentMap
  (asm [x] (mapcat asm x))
  String
  (asm [x] (mapcat asm x))
  Character
  (asm [c] [:PRED #(= c %)])
  clojure.lang.Keyword
  (asm [k] [:CALL k]))

(defn alt [& choices]
  (let [end (gensym :altend_)
        emit (fn emit [choices]
               (when-let [[choice & choices] (seq choices)]
                 (if choices
                   (let [addr (gensym :alt_)] (concat [:FORK addr]  (asm choice) [:JUMP end :LABEL addr] (emit choices)))
                   (asm choice))))] 
    (->InlineAsm (concat (emit choices) [:LABEL end]))))

(defn * [& xs]
  (let [start (gensym :star_)
        end (gensym :starend_)]
    (->InlineAsm (concat [:LABEL start :FORK end] (mapcat asm xs) [:JUMP start :LABEL end]))))

(defn + [& xs]
  (let [start (gensym :plus_)
        end (gensym :plusend_)]
    (->InlineAsm (concat [:LABEL start] (mapcat asm xs) [:FORK end :JUMP start :LABEL end]))))

(defn ? [& xs]
  (let [end (gensym :questionmarkend_)]
    (->InlineAsm (concat [:FORK end] (mapcat asm xs) [:LABEL end]))))

(defn as [tag & xs]
  (let [start (gensym :as_)
        end (gensym :asend_)]
    (->InlineAsm (concat [:CALL start :JUMP end :LABEL start] (mapcat asm xs) [:RET tag :LABEL end]))))

(defn grammar
  [start prods]
  (list* :JUMP start (asm prods)))
