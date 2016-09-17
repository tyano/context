(ns context.core
  (:refer-clojure :exclude [resolve map flatten] :as core)
  (:require [clojure.tools.logging :refer [debug info]]))

(defprotocol Context
  (result [_] "return the value which this context is holding."))

(defprotocol Functor
  (map [_ f]  "apply the value contained in this context to f, wrap the result of f into a new same context."))

(defprotocol Monad
  (bind [_ f] "apply the value contained in this context to f. f must return a new same-kind Context. return the new Context."))

(extend-type nil
  Context
  (result [this] this)

  Functor
  (map [this f] (f this))

  Monad
  (bind [this f] (f this)))


(extend-type Object
  Context
  (result [this] this)

  Functor
  (map [this f] (f this))

  Monad
  (bind [this f] (f this)))

(extend-type clojure.lang.PersistentVector
  Context
  (result [this] (vec this))

  Functor
  (map [this f] (vec (map f this)))

  Monad
  (bind [this f] (vec (mapcat f this))))


(declare maybe?)

(deftype Maybe [v]
  Context
  (result [this] v)

  Functor
  (map
    [this f]
    (if (some? v)
      (Maybe. (f v))
      this))

  Monad
  (bind
    [this f]
    (debug "f:" f)
    (debug "v:" v)
    (if (some? v)
      (let [new-context (f v)]
        (when-not (maybe? new-context)
          (throw (IllegalStateException. "a result value of f binded to a Maybe must be a Maybe.")))
        new-context)
      this)))

(defmethod print-method Maybe
  [o ^java.io.Writer w]
  (.write w (str (.getCanonicalName (class o)) "[" (.-v o) "]")))

(defn maybe? [c] (instance? Maybe c))

(def none (Maybe. nil))

(defn maybe [v]
  (if (some? v)
    (Maybe. v)
    none))

(defn flatten-maybe
  [c]
  (let [value (result c)]
    (if (instance? Maybe value)
      (recur value)
      value)))

(defn chain
  [m & fs]
  (reduce #(bind %1 %2) m fs))

(defn resolve
  [m & fs]
  (-> (apply chain m fs)
      (result)))

(defmacro chain->
  [m & body]
  (letfn [(parse-expr
            [[h & r :as expr]]
            `(fn [v#] (~h v# ~@r)))]
    (let [expr-coll (map parse-expr body)]
      `(chain ~m ~@expr-coll))))

(defmacro resolve->
  [m & body]
  `(result (chain-> ~m ~@body)))

(defn- expand-context
  [{:keys [syms variables expr]}]
  (if (seq syms)
    (let [[sym & symr] syms
          [v & vr] variables
          next-expr (expand-context {:syms symr :variables vr :expr expr})]
      `(bind ~sym (fn [~v] ~next-expr)))
    `~expr))

(defn- expand-binding
  [{:keys [sym syms variable variables expr] :as ctx}]
  `[~sym ~(expand-context ctx)])

(defn- build-binding-context
  [first-data data-coll]
  (reduce
    (fn [results pair]
      (conj results {:sym (gensym), :variable (first pair) :syms (map :sym results) :variables (map :variable results) :expr (second pair)}))
    first-data
    data-coll))

(defmacro expand-let
  [binding-context body-context]
  (if-not (seq binding-context)
    `(result ~(expand-context body-context))
    `(let ~(expand-binding (first binding-context))
        (expand-let ~(rest binding-context) ~body-context))))

(defmacro clet
  "(clet [[v1 v2] (maybe [1 2])
          [v3 v4] (vector (inc v1) (inc v2))]
     (+ v1 v2 v3 v4))

   => 8"
  [bindings & body]
  (let [[[variable expr] & others] (partition 2 bindings)
        binding-context (build-binding-context [{:sym (gensym), :variable variable :syms [], :variables [], :expr expr}] others)
        body-context    {:syms (map :sym binding-context), :variables (map :variable binding-context) :expr `(do ~@body)}]
    `(expand-let ~binding-context ~body-context)))
