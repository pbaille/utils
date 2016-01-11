(ns utils.colls)

(defn flat-reductions
  "almost copy/paste of clojure.core/reductions
   but concat sequentials reductions into result
   while continuing to reduce with the last element of the sequential"
  ([f coll]
   (lazy-seq
     (if-let [s (seq coll)]
       (flat-reductions f (first s) (rest s))
       (list (f)))))
  ([f init coll]
   (cons init
         (lazy-seq
           (when-let [s (seq coll)]
             (let [nxt (f init (first s))]
               (if (sequential? nxt)
                 (concat (butlast nxt) (flat-reductions f (last nxt) (rest s)))
                 (flat-reductions f nxt (rest s)))))))))

(defn rotate
  "rotate a collection. if n < 0 rotate backward"
  [coll n]
  (let [c (count coll)
        n (if (>= n 0) (mod n c) (+ n (count coll)))
        splited (split-at n coll)]
    (concat (splited 1) (splited 0))))

(defn in?
  "true if seq contains elm"
  ([seq] #(in? seq %))
  ([seq elm] (some #(= elm %) seq)))

(defn indexed [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos
  "return the seq of indexes of coll where pred is true"
  [pred coll]
  (for [[i v] (indexed coll) :when (pred v)] i))

(defn index-of [item coll] (first (pos #{item} coll)))

(defn count= [coll c] (-> coll count (= c)))

(defn fill-with [coll size el]
  (let [tail (repeat (- size (count coll)) el)]
    (into coll tail)))

(defn seq1
  "force 1 by one evaluation  of lazy sequence s"
  [s]
  (lazy-seq
    (when-let [[x] (seq s)]
      (cons x (seq1 (rest s))))))

(defn partition-if
  "split coll where (pred elem next-elem) is true
  TODO good implementation :) "
  [pred coll]
  (lazy-seq
    (when (second coll)
      (let [temp (take-while
                   #(not (pred (first %) (second %)))
                   (partition 2 1 coll))
            lst (if (seq temp) (last (last temp)) (first coll))
            run (concat (mapv first temp) [lst])
            nxt (seq (drop (count run) coll))
            cnxt (count nxt)]
        (cond
          (>= cnxt 2) (cons run (partition-if pred nxt))
          (= cnxt 1) (list run (list (last coll)))
          :else (list run))))))

(defn tails
  "like haskell's Data.List tails
  ex:
  (tails [1 2 3 4])
  => ([1 2 3 4] [2 3 4] [3 4] [4] [])"
  [xs]
  (let [f (cond
            (vector? xs) vec
            (set? xs) set
            :else seq)]
    (map f (take (inc (count xs)) (iterate rest xs)))))

(defn inits
  "like haskell's Data.List inits
  ex:
  (inits [1 2 3 4])
  => (() (1) (1 2) (1 2 3))"
  [xs]
  (for [n (range (count xs))]
    (take n xs)))

(defn all-distinct?
  "true if coll contains only distinct values"
  [coll]
  (if (seq coll) (apply distinct? coll) true))

(defn all-eq?
  "true if all item of coll are eq"
  [coll]
  (= 1 (count (set coll))))

(defn flat-homo-nested
  "flat nested coll of same type than x"
  [x]
  (let [tx (type x)
        of-type-tx? #(instance? tx %)]
    (filter #(not of-type-tx?)
            (rest (tree-seq of-type-tx? seq x)))))

(defn repeater
  "(repeater [[2 [3 [2 :foo] :bar] :woz] :bim [2 :yo]])
  => (:foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :foo
      :foo :bar :foo :foo :bar :foo :foo :bar :woz :bim :yo :yo)"
  [coll]
  (mapcat (fn [[n & els]]
            (if (count= els 1)
              (repeat n (first els))
              (apply concat (repeat n (mapcat #(repeater [%]) els)))))
          (map #(if (vector? %) % (vector 1 %)) coll)))
