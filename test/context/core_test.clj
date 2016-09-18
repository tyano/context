(ns context.core-test
  (:refer-clojure :exclude [map resolve])
  (:require [context.core :refer :all]
            [context.maybe :refer [none just]]
            [midje.sweet :refer [fact facts provided throws] :as m]
            [clojure.string :refer [upper-case]]))


(fact "tests for map"
  (->> [1 2 3]
       (map #(repeat % "A"))
       (result)) => [["A"] ["A" "A"] ["A" "A" "A"]])


(fact "tests for bind"
  (-> [:a :b :c]
      (bind #(vector (name %)))
      (bind #(vector (upper-case %)))) => ["A" "B" "C"]

  (-> [1 2 3]
      (bind #(vec (repeat % "A")))) => ["A" "A" "A" "A" "A" "A"]

  (-> [:a :b :c]
      (bind #(if (= % :b) nil [(name %)]))) => ["a" "c"])


(facts "for chain"
  (chain (just 1)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => (just 2)

  (chain (just 1)
    (fn [_] nil)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => none)

(facts "for chain->"
  (chain-> (just 1)
    (+ 1)
    (* 2)) => (just 4)

  (chain-> (just 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => none)

(facts "for maplet"
  (maplet [[v1 v2] (just [1 2])
           [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4)) => 8

  (maplet [v1 (just 1)
           v2 (just v1)
           v3 (map inc v2)]
    v3) => (just 2))

