(ns utils.maps)

(defn map->sorted
  "given a hash-map return the corresponding sorted-map"
  [m] (apply sorted-map (apply concat m)))


(defn sorted-map-by*
  "given a hash-map and a function return the corresponding sorted-map-by
  the advantage over regular sorted-map-by is that f takes keyvals instead of keys"
  ([f m] (apply sorted-map-by
                #(f [%1 (%1 m)] [%2 (%2 m)])
                (apply concat m)))
  ; sorted by result of (ctor (f val1)(f val2))
  ([ctor f m] (apply sorted-map-by
                     #(ctor (f (%1 m)) (f (%2 m)))
                     (apply concat m))))

(defn if-nil-merge [m1 m2]
  (merge-with #(if (or %1 %2) (if %1 %1 %2) nil) m1 m2))

(def h-map hash-map)

(defn entries->h-map
  "(entries->h-map [[:a 1][:b 2]]) => {:a 1 :b 2}"
  [tups-coll]
  (apply hash-map (apply concat tups-coll)))

(defn dissoc-in [m key-vec]
  (let [firsts (vec (butlast key-vec))
        node (dissoc (get-in m firsts) (last key-vec))]
    (assoc-in m firsts node)))

(defn submap? [sub m]
  (clojure.set/subset? (set sub) (set m)))

(defn in>
  "find the first value for kw key in map or nested maps"
  [m kw]
  (when (map? m)
    (if-let [v (kw m)]
      v (in> (apply merge (filter map? (vals m))) kw))))

(defn map-vals [f m]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn map-keys [f m]
  (apply merge (map (fn [[k v]] {(f k) v}) m)))

(defn map-h
  "map over an hash-map and return a new hash-map
  f must return either a single keyval map or a duplet (vec of size 2)"
  [f m]
  {:pre [(map? m)]}
  (let [hms (map (fn [[k v]] (f k v)) m)]
    (if (vector? (first hms))
      (entries->h-map hms)
      (reduce conj {} hms))))

(defn map-h*
  "same as map-h but f is single arity and is applied to each key and val
  (map-h* inc {1 2 3 4}) <=> (map-h #(h-map (inc %) (inc %2)) {1 2 3 4})"
  [f m]
  (map-h (fn [k v] (vector (f k) (f v))) m))

(defn dissoc-nils
  "remove keyvals whose val is nil from a h-map"
  [m]
  (when m (map-h (fn [k v] (if v {k v} {})) m)))

(declare first-truthy type=)

(defn key-path
  "find the deepless path of given key in a map"
  ([k coll] (key-path k [] coll))
  ([k pathv coll]
   (if (get-in coll (conj pathv k))
     (conj pathv k)
     (first-truthy
       #(when (type= % clojure.lang.MapEntry)
         (when (map? (val %)) (key-path k (conj pathv (key %)) coll)))
       (get-in coll pathv)))))

(defn filt-h
  "like filter but takes and return hash-map
   if pred fail on a keyval it is dissoc from h"
  [pred h]
  (reduce (fn [acc el]
            (if-not (pred el)
              (dissoc acc (key el))
              acc))
          h
          h))

(defn dissoc-if
  "same as filt-h but with args in reverse order to match with dissoc args order"
  [h pred] (filt-h pred h))

(defn rem-h [pred h]
  "like remove but takes and return h-map (see filt-h)"
  (filt-h (complement pred) h))
