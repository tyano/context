(ns context.maybe-test
  (:refer-clojure :exclude [map resolve])
  (:require [context.core :refer :all]
            [context.maybe :refer :all]
            [midje.sweet :refer [fact facts provided throws]]
            [clojure.string :refer [upper-case]]))

(facts "Maybe context"
  (just nil) => none
  (maybe? none) => true)

(fact "tests for map"
  (->> (just 1)
       (map inc)
       (map inc)
       (map #(* % 2))) => (just 6)

  (->> (just "a")
       (map #(drop 1 %))
       (map seq)
       (map upper-case)) => none)

(fact "tests for bind"
  (-> (just 1)
      (bind #(just (inc %)))
      (bind #(just (inc %)))
      (bind #(just (* % 2)))) => (just 6)

  (-> (just 1)
      (bind #(just (inc %)))
      (bind (fn [_] none))
      (bind #(just (* % 2)))) => none

  (-> (just 1)
      (bind inc)) => (throws IllegalStateException))

(facts "Monad lows"
  (-> (just 1)
    (bind #(just (inc %)))
    (result))
      => #(= % (inc 1))

  (-> (just 1) (bind just)) => (just 1)

  (bind> (just 1) #(just (inc %)) #(just (* % 2)))
      => #(= %
             (bind> (just 1)
                (fn [x] (bind> (just (inc x))
                               (fn [y] (just (* y 2))))))))
