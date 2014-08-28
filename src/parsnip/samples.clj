(ns parsnip.samples
  (:refer-clojure :exclude [+ *])
  (:require [parsnip.core :refer :all]
    [parsnip.vm :as vm]
    [parsnip.asm :as asm]))

(def pgm
  (asm/link
    (grammar :E
      {:E (alt "x" ["(" (* :E) ")"])})))

(def pgm2
  (asm/link
    (grammar :E
      {:E- (alt :X (as "X+E" [:X "+" :E]))
       :X- (alt (as "X" "x") (as "(E)" "(" :E ")"))})))

(def pgm3
  (asm/link
    (grammar :E
      {:E (alt :XX [:X :X])
       :XX "xx"
       :X "x"})))

#_#_#_
=> ((parser :E
         {:E- (alt :X (as "X+E" [:X "+" :E]))
          :X- (alt (as "X" "x") (as "(E)" "(" :E ")"))}
         )"x+x")
[{:tag "X+E", :content [{:tag "X", :content ["x"]} "+" {:tag "X", :content ["x"]}]}]


#_(let [step (vm/stepper pgm2)]
    (:carry (get (reduce-kv  step (step) (vec "(x+(x+x)+x)")) -1)))

#_(let [step (vm/stepper pgm2)]
    (reduce-kv  step (step) (vec "(x+x")))

#_(dotimes [_ 10] (time (let [step (vm/stepper pgm2)]
                          (:carry (get (reduce-kv  step (step) (vec (cons \x (take 2048 (cycle "+x"))))) -1))
                          )))

#_(dotimes [_ 10] (time (let [step (vm/stepper pgm2)]
                          (get (reduce-kv  step (step) (into (vec (repeat 1024 \()) (cons \x (repeat 1024 \))))) -1)
                          nil)))



