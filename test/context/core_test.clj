(ns context.core-test
  (:refer-clojure :exclude [map resolve])
  (:require [clojure.core :as core]
            [context.core :refer :all]
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
      (bind #(if (= % :b) [] [(name %)]))) => ["a" "c"])


(facts "for map>"
  (map> (just 1)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => (just 2)

  (map> (just 1)
    (fn [_] nil)
    #(+ % 1)
    #(+ % 1)
    #(- % 1)) => none)

(facts "for map->"
  (map-> (just 1)
    (+ 1)
    (* 2)) => (just 4)

  (map-> (just 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => none)

(facts "for map->>"
  (map->> (just [:a :b :c])
    (core/map name)
    (core/map upper-case)) => (just '("A" "B" "C"))

  (map->> (just [:a :b :c])
    (core/map name)
    ((fn [_] nil))
    (core/map upper-case) => none))

(facts "for maplet"
  (maplet [[v1 v2] (just [1 2])
           [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4)) => (just 8)

  (maplet [v1 (just 1)
           v2 (just v1)
           v3 (map inc v2)]
    v3) => (just (just 2)))

(facts "for bind>"
  (bind> (just 1)
    #(just (+ % 1))
    #(just (+ % 1))
    #(just (- % 1))) => (just 2)

  (bind> (just 1)
    (fn [_] none)
    #(just (+ % 1))
    #(just (+ % 1))
    #(just (- % 1)))=> none)

(facts "for bind->"
  (bind-> (just 1)
    (+ 1)
    (* 2)) => (just 4)

  (bind-> (just 1)
    ((fn [_] nil))
    (+ 1)
    (* 2)) => none)

(facts "for bind->>"
  (bind->> (just [:a :b :c])
    (core/map name)
    (core/map upper-case)) => (just '("A" "B" "C"))

  (bind->> (just [:a :b :c])
    (core/map name)
    ((fn [_] nil))
    (core/map upper-case) => none))


(facts "for bindlet"
  (bindlet [[v1 v2] (just [1 2])
            [v3 v4] (just (vector (inc v1) (inc v2)))]
     (just (+ v1 v2 v3 v4))) => (just 8)

  (bindlet [v1 (just 1)
            v2 (just v1)
            v3 (just (inc v2))]
    (just v3)) => (just 2)

  (bindlet [v1 (just 1)
            v2 (just (+ v1 1))]
    (just (* v2 2))) => (just 4)

  (bindlet [v1 (just 1)
            v2 none
            v3 (just (+ v1 v2))]
    (just v3)) => none)

