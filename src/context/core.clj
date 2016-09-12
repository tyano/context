(ns context.core
  (:refer-clojure :exclude [resolve]))

(defprotocol Context
  (result [_] "return the value which this context is holding.")
  (bind [_ f] "apply the value which this context is holding to a function f, and return a new context."))

(extend-protocol Context
  nil
  (result [this] this)
  (bind [this f] (f this))

  Object
  (result [this] this)
  (bind [this f] (f this))

  clojure.lang.PersistentVector
  (result [this] (vec this))
  (bind [this f] (vec (map f this))))

(defrecord Maybe [v]
  Context
  (result [this] (result v))
  (bind
    [this f]
    (if-some [r (result v)]
      (Maybe. (f r))
      this)))

(defn maybe [v] (Maybe. v))

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
