(ns context.core-test
  (:refer-clojure :exclude [map resolve] :as core)
  (:require [clojure.test :refer :all]
            [context.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.string :refer [upper-case]]))

(facts "Maybe context"
  (fact "tests for map"
    (-> (maybe 1) (map inc) (map inc) (map #(* % 2)) (result)) => 6
    (-> (maybe "a") (map #(drop 1 %)) (map seq) (map upper-case) (result)) => nil)

  (fact "tests for bind"
    (-> (maybe 1) (bind #(maybe (inc %))) (bind #(maybe (inc %))) (bind #(maybe (* % 2))) (result)) => 6
    (-> (maybe 1) (bind #(maybe (inc %))) (bind (fn [_] none)) (bind #(maybe (* % 2))) (result)) => nil
    (-> (maybe 1) (bind inc)) => (throws IllegalStateException)))
