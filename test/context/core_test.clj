(ns context.core-test
  (:refer-clojure :exclude [map resolve] :as core)
  (:require [clojure.test :refer :all]
            [context.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.string :refer [upper-case]]))

(facts "Maybe context"
  (maybe nil) => none

  (fact "tests for map"
    (->> (maybe 1)
         (map inc)
         (map inc)
         (map #(* % 2))
         (result)) => 6

    (->> (maybe "a")
         (map #(drop 1 %))
         (map seq)
         (map upper-case)
         (result)) => nil

    (->> [1 2 3]
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

(facts "for chain"
  (chain (maybe 1)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => (maybe 2)

  (chain (maybe 1)
    (fn [_] nil)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => none)

(facts "for resolve"
  (resolve (maybe 1)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => 2

  (resolve (maybe 1)
    (fn [_] nil)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => nil)

(facts "for chain->"
  (chain-> (maybe 1)
    (+ 1)
    (* 2)) => (maybe 4)

  (chain-> (maybe 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => none)

(facts "for resolve->"
  (resolve-> (maybe 1)
    (+ 1)
    (* 2)) => 4

  (resolve-> (maybe 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => nil)

(facts "for maplet"
  (maplet [[v1 v2] (maybe [1 2])
           [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4)) => 8

  (maplet [v1 (maybe 1)
           v2 (maybe v1)
           v3 (map inc v2)]
    v3) => (maybe 2))

(facts "Monad lows"
  (-> (maybe 1) (bind #(maybe (inc %))) (result))
      => #(= % (inc 1))

  (-> (maybe 1) (bind maybe)) => (maybe 1)

  (-> (maybe 1) (bind #(maybe (inc %))) (bind #(maybe (* % 2))))
      => (fn [v]
          (= v
             (-> (maybe 1)
                 (bind (fn [x] (-> (maybe (inc x))
                                   (bind (fn [y] (maybe (* y 2)))))))))))

