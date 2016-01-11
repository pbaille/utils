(ns utils.prob
  (:require [utils.hof :refer [select-first]]))

(defn weight-picker [m]                                     ;map of object/prob pairs
  (let [sums (reductions + 0 (vals m))
        parts (map #(hash-map :obj %1 :min (first %2) :max (second %2))
                   (keys m)
                   (partition 2 1 sums))]
    (fn f
      ([]
       (let [x (rand (last sums))
             l (select-first #(<= (:min %) x (:max %)) parts)]
         (:obj l)))
      ([x] (take x (repeatedly f))))))

(defn weight-pick-one [m]                                   ;map of object/prob pairs
  ((weight-picker m)))
