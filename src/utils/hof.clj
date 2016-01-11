(ns utils.hof
  "higher order functions")

(defn map-reduce [f init coll]
  (next (reductions f init coll)))

(defn map-with-coll [f coll]
  "like map but f takes extra argument coll"
  (map f coll (repeat coll)))

(defn red-with-coll [f init coll]
  "same as reduce but takes a function that takes 3 arguments [acc el coll]"
  (reduce #(f %1 %2 coll) init coll))

(defn best [f coll]
  (reduce #(if (f %1 %2) %1 %2) coll))

(defn select-first [pred coll]
  (first (filter pred coll)))

(defn map-nth
  "apply f on each nth elems of coll"
  [n f coll]
  (mapcat #(apply vector (f (first %)) (next %))
          (partition n n nil coll)))

(defn filt-map [ff mf coll]
  "map coll with mf then filter results with ff"
  (filter ff (map mf coll)))

(defn map-filt [mf ff coll]
  "filter coll with ff then map results with mf"
  (map mf (filter ff coll)))

(defn first-truthy [f coll]
  "take the first truthy element of (map f coll)
  (first-truthy #(when (< 0 %) %) [-1 2 3])
  => 2"
  (first (filter identity (map f coll))))

(defn set-map [f coll]
  "call set on the result of map"
  (set (map f coll)))

(defn drop-last-while
  "drop last items until pred is true
  vector only!
  (drop-last-while (partial < 10) [57 8 1 6 80 9 90 99 78])
  => (57 8 1 6 80 9) "
  [pred coll]
  (let [cnt (count (drop-while pred (reverse coll)))]
    (take cnt coll)))

(defn reduce-while
  "reduce coll while (pred acc)
  returns the last valid intermediate value of acc"
  [f pred init coll]
  (if (empty? coll)
    init
    (let [result (f init (first coll))]
      (if (pred result)
        (recur f pred result (rest coll))
        init))))

(defn iterate-while
  "like (last (take-while pred (iterate f init))) "
  [pred f init]
  (let [result (f init)]
    (if (pred result)
      (recur pred f result)
      init)))

(defn reduce-while-not-nil [f init coll]
  (reduce-while f (complement nil?) init coll))

(defn any?
  "return true if any elements of (apply map f colls) is truthy"
  [f & colls]
  (when (seq (remove #(or (false? %) (nil? %)) (apply map f colls))) true))
