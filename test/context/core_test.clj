(ns context.core-test
  (:refer-clojure :exclude [map resolve] :as core)
  (:require [clojure.test :refer :all]
            [context.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.string :refer [upper-case]]))

(facts "Maybe context"
  (fact "tests for map"
    (-> (maybe 1)
        (map inc)
        (map inc)
        (map #(* % 2))
        (result)) => 6

    (-> (maybe "a")
        (map #(drop 1 %))
        (map seq)
        (map upper-case)
        (result)) => nil

    (-> [1 2 3]
        (map #(repeat % "A"))
        (result)) => [["A"] ["A" "A"] ["A" "A" "A"]])

  (fact "tests for bind"
    (-> (maybe 1)
        (bind #(maybe (inc %)))
        (bind #(maybe (inc %)))
        (bind #(maybe (* % 2)))
        (result)) => 6

    (-> (maybe 1)
        (bind #(maybe (inc %)))
        (bind (fn [_] none))
        (bind #(maybe (* % 2)))
        (result)) => nil

    (-> (maybe 1)
        (bind inc)) => (throws IllegalStateException)

    (-> [:a :b :c]
        (bind #(vector (name %)))
        (bind #(vector (upper-case %)))
        (result)) => ["A" "B" "C"]

    (-> [1 2 3]
        (bind #(vec (repeat % "A")))
        (result)) => ["A" "A" "A" "A" "A" "A"]

    (-> [:a :b :c]
        (bind #(if (= % :b) nil [(name %)]))
        (result)) => ["a" "c"]))

(facts "for maplet"
  (maplet [[v1 v2] (maybe [1 2])
           [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4)) => 8

  (maplet [v1 (maybe 1)
           v2 (maybe v1)
           v3 (map v2 inc)]
    v3) => (maybe 2))
