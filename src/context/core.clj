(ns context.core
  (:refer-clojure :exclude [resolve]))

(defprotocol Context
  (result [_] "return the value which this context is holding.")
  (bind [_ f] "apply the value which this context is holding to a function f, and return a new context."))

(defn context?
  [c]
  (satisfies? Context c))

(defrecord Maybe [v]
  Context
  (result [this] v)
  (bind [this f] (if (some? v) (->Maybe (f v)) this)))

(defn maybe [v] (Maybe. v))

(extend-type clojure.lang.PersistentVector
  Context
  (result [this] this)
  (bind [this f] (vec (flatten (map f this)))))

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

; (clet c [v  (+ 1 c)
;          v2 (* 2 v)]
;   (identity v2))

(defn expand-context
  [{:keys [syms expr]}]
  (if (seq syms)
    (let [[sym & r] syms
          next-expr (expand-context {:syms r :expr expr})]
      `(bind ~sym (fn [~sym] ~next-expr)))
    `~expr))


; (let [v (bind c (fn [c] (+ 1 c)))]
;   (let [v2 (bind c (fn [c] (bind v (fn [v] (* 2 v)))))]
;     (bind c
;       (fn [c]
;         (bind v
;           (fn [v]
;             (bind v2
;               (fn [v2]
;                 (identity v2)))))))))