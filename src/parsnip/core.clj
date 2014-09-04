(ns parsnip.core
  (:refer-clojure :exclude [+ *])
  (:require [parsnip.asm :as asm]
    [parsnip.vm :as vm]))

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

(defn simple-tree-builder [s]
  (fn
    ([] {:offset 0 :stack (list []) :current []})
    ([state] (:current state))
    ([state [op pos tag]]
      (case op
        :push (let [from (:offset state)
                    current (:current state)
                    current (if (< from pos) (conj current (subs s from pos)) current)]
                (assoc state :offset pos :stack (conj (:stack state) current) :current []))
        :pop (let [from (:offset state)
                   current (:current state)
                   current (if (< from pos) (conj current (subs s from pos)) current)
                   stack (:stack state)
                   current (if tag
                             (conj (peek stack) {:tag tag :content current})
                             (into (peek stack) current))]
               (assoc state :offset pos :stack (pop stack) :current current))
        :skip (assoc state :offset pos :current (conj (:current state) {:tag :skip :content [(subs s pos (inc pos))]}))))))

(defn parser [start prods]
  (let [step (vm/stepper (asm/link (grammar start prods)))]
    (fn self
      ([input] (self input (simple-tree-builder input)))
      ([input builder]
        (builder (reduce builder (builder) (doto (:events (:carry (get (reduce-kv  step (step) (vec input)) -2))) prn)))))))