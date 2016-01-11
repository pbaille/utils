(ns utils.numbers
  (:require [clojure.math.numeric-tower :as math]
            [utils.hof :refer [best]]))

(defn mod12 [x] (mod x 12))
(defn mod12map [coll] (map mod12 coll))
(defn abs [x] (if (neg? x) (* x -1) x))
(defn rand-int-between [a b] (rand-nth (range a b)))
(defn div-mod [x div] [(int (/ x div)) (mod x div)])
(defn int-div [x div] (int (/ x div)))
(defn same-sign? [x y] (pos? (* x y)))
(defn dist [x y] (abs (- x y)))

(defn median
  ([coll] (/ (reduce + coll) (count coll)))
  ([x & xs] (median (cons x xs))))

(defn opposite-sign? [x y]
  (or (and (pos? x) (neg? y))
      (and (neg? x) (pos? y))))

(defn round
  ([n] (math/round n))
  ([s n] (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)))

(defn between
  ([a b] (between a (first b) (second b)))
  ([a b1 b2] (and (>= a b1) (<= a b2))))

(defn prime-factors [n]
  (loop [n n divisor 2 factors []]
    (if (< n 2)
      factors
      (if (= 0 (rem n divisor))
        (recur (/ n divisor) divisor (conj factors divisor))
        (recur n (inc divisor) factors)))))

(defn steps
  "return steps between each adjacent items of coll"
  [coll]
  (reduce #(conj %1 (apply - (reverse %2))) [] (partition 2 1 coll)))

(defn steps-bounds
  "return min and max amplitude of step-sequence"
  [steps]
  (let [mr (reductions + 0 steps)]
    [(apply min mr) (apply max mr)]))

;from overtone
(defn scale-range
  "Scales a given input value within the specified input range to a
  corresponding value in the specified output range using the formula:

           (out-max - out-min) (x - in-min)
   f (x) = --------------------------------  + out-min
                    in-max - in-min
  "
  [x in-min in-max out-min out-max]
  (+ (/ (* (- out-max out-min) (- x in-min))
        (- in-max in-min))
     out-min))

(defn range-scaler [min-in max-in min-out max-out]
  #(scale-range % min-in max-in min-out max-out))

(defn range-by
  "max inclusive"
  ([end step] (range-by 0 end step))
  ([start end step]
   (let [incf (if (> start end) - +)
         amp (math/abs (- end start))]
     (map #(-> (* % step) incf (+ start))
          (range 0 ((comp inc int) (/ amp step)))))))

(defn min-max
  "takes a seq of numbers and return a vector [min max]"
  [coll]
  ((juxt #(apply min %) #(apply max %)) coll))

(defn closest
  "takes a target (num) and a coll [(num)]
  returns the closest to target elem of coll"
  [target num-coll]
  (best #(< (dist target %1) (dist target %2)) num-coll))
