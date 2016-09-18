(ns context.core
  (:refer-clojure :exclude [resolve map flatten])
  (:require [clojure.core :as core]
            [clojure.tools.logging :refer [debug info]]))

(defprotocol Context
  (result [_] "return the value which this context is holding."))

(defprotocol Functor
  (fmap [_ f]  "apply the value contained in this context to f, wrap the result of f into a new same context."))

(defprotocol Monad
  (bind [_ f] "apply the value contained in this context to f. f must return a new same-kind Context. return the new Context."))

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

(extend-type clojure.lang.PersistentVector
  Context
  (result [this] (vec this))

  Functor
  (fmap [this f] (vec (core/map f this)))

  Monad
  (bind [this f] (vec (mapcat f this))))

(extend-type Object
  Context
  (result [this] this)

  Functor
  (fmap [this f] (f this))

  Monad
  (bind [this f] (f this)))

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

(defmethod print-method Maybe
  [^Maybe o ^java.io.Writer w]
  (.write w (str (.getCanonicalName (class o)) "[" (.-v o) "]")))

(defn maybe? [c] (instance? Maybe c))

(defonce none (Maybe. nil))

(defn just [v]
  (if (some? v)
    (Maybe. v)
    none))

(defn chain
  [m & fs]
  (reduce #(fmap %1 %2) m fs))

(defn resolve
  [m & fs]
  (-> (apply chain m fs)
      (result)))

(defmacro chain->
  [m & body]
  (letfn [(parse-expr
            [[h & r :as expr]]
            `(fn [v#] (~h v# ~@r)))]
    (let [expr-coll (core/map parse-expr body)]
      `(chain ~m ~@expr-coll))))

(defmacro resolve->
  [m & body]
  `(result (chain-> ~m ~@body)))

(defn- expand-context
  ([first? {:keys [syms variables expr]}]
    (if (seq syms)
      (let [[sym & symr] syms
            [v & vr] variables
            next-expr (expand-context false {:syms symr :variables vr :expr expr})]
        (if first?
          `(fmap ~sym (fn [~v] ~next-expr))
          `(result (fmap ~sym (fn [~v] ~next-expr)))))
      `~expr))
  ([ctx]
    (expand-context true ctx)))

(defn- expand-binding
  [{:keys [sym syms variable variables expr] :as ctx}]
  `[~sym ~(expand-context ctx)])

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

(defmacro expand-let
  [binding-context body-context]
  (if-not (seq binding-context)
    `(result ~(expand-context body-context))
    `(let ~(expand-binding (first binding-context))
        (expand-let ~(rest binding-context) ~body-context))))

(defmacro maplet
  "(maplet [[v1 v2] (maybe [1 2])
            [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4))

   => 8"
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
    `(expand-let ~binding-context ~body-context)))
