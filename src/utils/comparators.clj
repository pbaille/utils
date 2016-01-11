(ns utils.comparators)

(defn eq
  ([v1 v2]
   (= 0 (compare v1 v2)))
  ([x y & xs] (every? (partial apply eq) (partition 2 1 (cons x (cons y xs))))))

(defn lt
  ([v1 v2]
   (= -1 (compare v1 v2)))
  ([x y & xs]
   (every? (partial apply lt)
           (partition 2 1 (cons x (cons y xs))))))

(defn gt
  ([v1 v2]
   (= 1 (compare v1 v2)))
  ([x y & xs]
   (every? (partial apply gt)
           (partition 2 1 (cons x (cons y xs))))))

(defn gte
  ([v1 v2]
   (let [c (compare v1 v2)]
     (or (= 0 c) (= 1 c))))
  ([x y & xs]
   (every? (partial apply gte)
           (partition 2 1 (cons x (cons y xs))))))

(defn lte
  ([v1 v2]
   (let [c (compare v1 v2)]
     (or (= 0 c) (= -1 c))))
  ([x y & xs]
   (every? (partial apply lte)
           (partition 2 1 (cons x (cons y xs))))))
