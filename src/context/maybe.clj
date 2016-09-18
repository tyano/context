(ns context.maybe
  (:require [context.core :refer [Context Functor Monad result return]]))

(declare just maybe? none)

(deftype Maybe [v]
  Context
  (result [this] v)

  Functor
  (fmap
    [this f]
    (if (= this none)
      none
      (just (f v))))

  Monad
  (bind
    [this f]
    (if (= this none)
      none
      (let [new-context (f v)]
        (when-not (maybe? new-context)
          (throw (IllegalStateException. "a result value of f binded to a Maybe must be a Maybe.")))
        new-context)))

  Object
  (equals
    [a b]
    (cond
      (identical? a b) true
      (not (maybe? b)) false
      :else (= (result a) (result b)))))

(defmethod return Maybe [c v] (just v))

(defmethod print-method Maybe
  [^Maybe o ^java.io.Writer w]
  (.write w (str (.getCanonicalName (class o)) "[" (.-v o) "]")))

(defn maybe? [c] (instance? Maybe c))

(defonce none (Maybe. nil))

(defn just [v]
  (if (some? v)
    (Maybe. v)
    none))
