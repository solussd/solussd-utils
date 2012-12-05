(ns solussd-utils.core)

(defmacro sub-if
  "If (pred expr) returns true, returns subexpr, otherwise expr
Optionally, you can provide an anaphor to access the result of expr in subexpr"
  ([expr pred subexpr anaphor]
     `(let [~anaphor ~expr]
        (if (~pred ~anaphor)
          ~subexpr
          ~anaphor)))
  ([expr pred subexpr]
     (let [sym (gensym)]
       `(sub-if ~expr ~pred ~subexpr ~sym))))

(defmacro if-all-let
  "Same as if-let, but supports multiple bindings. The else condition is evaluated if any binding is falsey"
  ([bindings then else]
     (reduce (fn [subform binding]
               `(if-let [~@binding] ~subform ~else))
             then (reverse (partition 2 bindings))))
  ([bindings then]
     `(if-all-let ~bindings ~then nil)))

(defn bound-seq*
  [bind-map inner-seq]
  (lazy-seq
    (with-bindings bind-map
      (when-let [s (seq inner-seq)]
        (cons (first s) (bound-seq* bind-map (rest s)))))))

(defmacro bound-seq
  ([inner-seq]
   `(bound-seq* (get-thread-bindings) ~inner-seq))
  ([bind-map inner-seq]
   `(bound-seq* (hash-map ~@(mapcat (fn [[k v]] [`(var ~k) v]) bind-map))
                ~inner-seq)))

(defmacro bound-lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed.

  (bound-lazy-cat xs ys zs) === (concat (bound-seq xs) (bound-seq ys) (bound-seq zs))"
  {:comment "Derived from lazy-cat"}
  [& colls]
  `(concat ~@(map #(list `bound-seq %) colls)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  {:comment "From clojure.contrib.map-utils, which is apparently mia in clojure 1.3+"}
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn get-all
  "Gets the value for a key, k, in a collection, coll, (like get).
If the collection is sequential, looks for the key in each of the collections components, recursively.
Preserves \"shape\" with respect to nested vectors
Examples:
 (get-all [{:a 8 :x 9} {:a 6 :y 7}] :a) => (8 6)
 (get-all [{:a 8 :x 9} {:a 6 :y 7} [{:d 9}]] :d) => ((9))
 (get-all {:a 9 :b 3} :b) => 3"
  [coll k]
  (if-let [found (get coll k)]
    found
    (if (sequential? coll)
      (seq (filter (complement nil?)
              (for [item coll]
                (get-all item k)))))))

(defn get-all-in
  "Like get-in, but uses get-all instead of get for reduction
Examples:
 (get-all-in {:a [{:b {:c 5}} {:b {:c 6}} [{:b {:c 9}}]]} [:a :b :c]) => (5 6 (9))
 (get-all-in {:a [{:b {:c 5 :d 10}} {:b {:c 6}} [{:b {:c 9}}]]} [:a :b :d]) => (10)
 (get-all-in {:a {:b {:c 7}}} [:a :b :c]) => 7"
  [coll ks]
  (reduce get-all coll ks))
