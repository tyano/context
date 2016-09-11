(ns context.core
  (:refer-clojure :exclude [resolve]))

(defprotocol Context
  (result [_] "return the value which this context is holding.")
  (bind [_ f] "apply the value which this context is holding to a function f, and return a new context."))

(extend-protocol Context
  Object
  (result [this] this)
  (bind [this f] (f this))

  clojure.lang.PersistentVector
  (result [this] this)
  (bind [this f] (vec (flatten (map f this)))))

(defrecord Maybe [v]
  Context
  (result [this] (result v))
  (bind
    [this f]
    (if (some? v)
      (Maybe. (f (result v)))
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
  `(resolve (chain-> ~m ~@body)))

(defn expand-context
  [{:keys [syms expr]}]
  (if (seq syms)
    (let [[sym & r] syms
          next-expr (expand-context {:syms r :expr expr})]
      `(bind ~sym (fn [~sym] ~next-expr)))
    `~expr))

(defn expand-binding
  [{:keys [sym syms expr] :as ctx}]
  `[~sym ~(expand-context ctx)])

(defn build-binding-context
  [first-data data-coll]
  (reduce
    (fn [results pair]
      (conj results {:sym (first pair) :syms (map :sym results) :expr (second pair)}))
    first-data
    data-coll))

(defmacro expand-let
  [binding-context body-context]
  (if-not (seq binding-context)
    `(result ~(expand-context body-context))
    `(let ~(expand-binding (first binding-context))
        (expand-let ~(rest binding-context) ~body-context))))

(defmacro clet
  "(clet [c  (maybe 1)
          v  (+ 1 c)
          v2 (* 2 v)]
     (+ c v v2))

   => 7"
  [bindings & body]
  (let [[[sym expr] & others] (partition 2 bindings)
        binding-context (build-binding-context [{:sym sym, :syms [], :expr expr}] others)
        body-context    {:syms (map :sym binding-context), :expr `(do ~@body)}]
    `(expand-let ~binding-context ~body-context)))
