(ns context.core-test
  (:refer-clojure :exclude [map resolve] :as core)
  (:require [context.core :refer :all]
            [midje.sweet :refer [fact facts provided throws] :as m]
            [clojure.string :refer [upper-case]]))

(facts "Maybe context"
  (just nil) => none
  (maybe? none) => true

  (fact "tests for map"
    (->> (just 1)
         (map inc)
         (map inc)
         (map #(* % 2))
         (result)) => 6

    (->> (just "a")
         (map #(drop 1 %))
         (map seq)
         (map upper-case)
         (result)) => nil

    (->> [1 2 3]
         (map #(repeat % "A"))
         (result)) => [["A"] ["A" "A"] ["A" "A" "A"]])

  (fact "tests for bind"
    (-> (just 1)
        (bind #(just (inc %)))
        (bind #(just (inc %)))
        (bind #(just (* % 2)))
        (result)) => 6

    (-> (just 1)
        (bind #(just (inc %)))
        (bind (fn [_] none))
        (bind #(just (* % 2)))
        (result)) => nil

    (-> (just 1)
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

(facts "for resolve"
  (resolve (just 1)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => 2

  (resolve (just 1)
    (fn [_] nil)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => nil)

(facts "for chain->"
  (chain-> (just 1)
    (+ 1)
    (* 2)) => (just 4)

  (chain-> (just 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => none)

(facts "for resolve->"
  (resolve-> (just 1)
    (+ 1)
    (* 2)) => 4

  (resolve-> (just 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => nil)

(facts "for maplet"
  (maplet [[v1 v2] (just [1 2])
           [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4)) => 8

  (maplet [v1 (just 1)
           v2 (just v1)
           v3 (map inc v2)]
    v3) => (just 2))

(facts "Monad lows"
  (-> (just 1) (bind #(just (inc %))) (result))
      => #(= % (inc 1))

  (-> (just 1) (bind just)) => (just 1)

  (-> (just 1) (bind #(just (inc %))) (bind #(just (* % 2))))
      => (fn [v]
          (= v
             (-> (just 1)
                 (bind (fn [x] (-> (just (inc x))
                                   (bind (fn [y] (just (* y 2)))))))))))

