(ns utils.futils_test
  (:require [clojure.test :refer :all]
            [utils.futils :refer :all]))

(testing "cfn"
  (is (= 2 (((cfn [a b] (+ a b)) 1) 1))))

(def-curried add2 [a b] (+ a b))

(testing "defcurried"
  (is (= 2 ((add2 1) 1))))

(testing "arg"
  (let [a (arg 1)]
    (is (= 2 (a inc)))))

(testing "args"
  (let [a2 (args 1 1)]
    (is (= 2 (a2 +)))))

(testing "arg-n"
  (is (= 1 (arg1 1 2 3 [:stuff] :pouet)))
  (is (= 2 (arg2 1 2 3 [:stuff] :pouet)))
  (is (= 3 (arg3 1 2 3 [:stuff] :pouet))))

(testing "zip2"
  (is (= (zip2 + [0 1 0 1] [1 0 1 0]) (list 1 1 1 1))))

(testing "flip"
  (is (= 1 (flip - 1 2))))

(testing ">>"
  (is (= "3" ((>> inc inc inc str) 0))))

(testing "applied"
  (is (= 2 ((applied +) [1 1]))))
