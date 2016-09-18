(ns context.core
  (:refer-clojure :exclude [map flatten])
  (:require [clojure.core :as core]
            [clojure.tools.logging :refer [debug info]]))

(defprotocol Context
  (result [_] "return the value which this context is holding."))

(defprotocol Functor
  (fmap [_ f]  "apply the value contained in this context to f, wrap the result of f into a new same context."))

(defprotocol Monad
  (bind [_ f] "apply the value contained in this context to f. f must return a new same-kind Context. return the new Context."))

(defmulti return (fn [clazz v] clazz))

(defn map
  [f data]
  (fmap data f))

(extend-type nil
  Context
  (result [this] nil)

  Functor
  (fmap [this f] nil)

  Monad
  (bind [this f] nil))

(defmethod return nil [_ _] nil)

(extend-type clojure.lang.PersistentVector
  Context
  (result [this] (vec this))

  Functor
  (fmap [this f] (vec (core/map f this)))

  Monad
  (bind [this f] (vec (mapcat f this))))

(defmethod return clojure.lang.PersistentVector [c v] (vec v))

(extend-type Object
  Context
  (result [this] this)

  Functor
  (fmap [this f] (f this))

  Monad
  (bind [this f] (f this)))

(defmethod return Object [c v] v)


(defn map-chain
  [m & fs]
  (reduce #(fmap %1 %2) m fs))

(defmacro map->
  [m & body]
  (letfn [(parse-expr
            [[h & r :as expr]]
            (let [s (gensym)]
              `(fn [~s] (~h ~s ~@r))))]
    (let [expr-coll (core/map parse-expr body)]
      `(map-chain ~m ~@expr-coll))))

(defmacro map->>
  [m & body]
  (letfn [(parse-expr
            [[& exprs]]
            (let [s (gensym)]
              `(fn [~s] (~@exprs ~s))))]
    (let [expr-coll (core/map parse-expr body)]
      `(map-chain ~m ~@expr-coll))))

(defn bind-chain
  [m & fs]
  (reduce #(bind %1 %2) m fs))



(defn expand-bindfirst-expr
  [m [h & r :as expr]]
  (let [s (gensym)]
    `(fn [~s] (return (class ~m) (~h ~s ~@r)))))

(defmacro expand-bindfirst-body
  [m body]
  (vec (core/map #(expand-bindfirst-expr m %) body)))

(defmacro bind->
  [m & body]
  `(let [m# ~m]
     (apply bind-chain m# (expand-bindfirst-body m# ~body))))



(defn expand-bindlast-expr
  [m [& exprs]]
  (let [s (gensym)]
    `(fn [~s] (return (class ~m) (~@exprs ~s)))))

(defmacro  expand-bindlast-body
  [m body]
  (vec (core/map #(expand-bindlast-expr m %) body)))

(defmacro bind->>
  [m & body]
  `(let [m# ~m]
     (apply bind-chain m# (expand-bindlast-body m# ~body))))



(defn- build-binding-context
  [first-data data-coll]
  (reduce
    (fn [results pair]
      (conj results {:sym (gensym),
                     :variable (first pair)
                     :syms (core/map :sym results)
                     :variables (core/map :variable results)
                     :expr (second pair)}))
    first-data
    data-coll))

(defn- maplet-expand-context
  ([first? {:keys [syms variables expr]}]
    (if (seq syms)
      (let [[sym & symr] syms
            [v & vr] variables
            next-expr (maplet-expand-context false {:syms symr :variables vr :expr expr})]
        (if first?
          `(fmap ~sym (fn [~v] ~next-expr))
          `(result (fmap ~sym (fn [~v] ~next-expr)))))
      `~expr))
  ([ctx]
    (maplet-expand-context true ctx)))

(defn- maplet-expand-binding
  [{:keys [sym syms variable variables expr] :as ctx}]
  `[~sym ~(maplet-expand-context ctx)])

(defmacro maplet-expand-let
  [binding-context body-context]
  (if-not (seq binding-context)
    (maplet-expand-context body-context)
    `(let ~(maplet-expand-binding (first binding-context))
        (maplet-expand-let ~(rest binding-context) ~body-context))))

(defmacro maplet
  "(maplet [[v1 v2] (just [1 2])
            [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4))

   => (just 8)"
  [bindings & body]
  (let [[[variable expr] & others] (partition 2 bindings)
        binding-context (build-binding-context [{:sym (gensym),
                                                 :variable variable
                                                 :syms []
                                                 :variables []
                                                 :expr expr}]
                                                others)
        body-context    {:syms (core/map :sym binding-context),
                                         :variables (core/map :variable binding-context)
                                         :expr `(do ~@body)}]
    `(maplet-expand-let ~binding-context ~body-context)))




(defn- bindlet-expand-context
  [{:keys [syms variables expr]}]
  (if (seq syms)
    (let [[sym & symr] syms
          [v & vr] variables
          next-expr (bindlet-expand-context {:syms symr :variables vr :expr expr})]
        `(bind ~sym (fn [~v] ~next-expr)))
    `~expr))

(defn- bindlet-expand-binding
  [{:keys [sym syms variable variables expr] :as ctx}]
  `[~sym ~(bindlet-expand-context ctx)])

(defmacro bindlet-expand-let
  [binding-context body-context]
  (if-not (seq binding-context)
    (bindlet-expand-context body-context)
    `(let ~(bindlet-expand-binding (first binding-context))
        (bindlet-expand-let ~(rest binding-context) ~body-context))))

(defmacro bindlet
  "(bindlet [[v1 v2] (just [1 2])
             [v3 v4] (just (vector (inc v1) (inc v2)))]
     (just (+ v1 v2 v3 v4)))

   => (just 8)"
  [bindings & body]
  (let [[[variable expr] & others] (partition 2 bindings)
        binding-context (build-binding-context [{:sym (gensym),
                                                 :variable variable
                                                 :syms []
                                                 :variables []
                                                 :expr expr}]
                                                others)
        body-context    {:syms (core/map :sym binding-context),
                                         :variables (core/map :variable binding-context)
                                         :expr `(do ~@body)}]
    `(bindlet-expand-let ~binding-context ~body-context)))
