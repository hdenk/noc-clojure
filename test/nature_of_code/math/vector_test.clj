(ns nature-of-code.math.vector-test
  (:use [clojure.test :only [deftest is testing]]
        [nature-of-code.test-utils :only [close-to]])
  (:require [nature-of-code.math.vector :as vector]))

(deftest add
    (testing 
      "add"
      (is 
        (=
          [3 5]
          (vector/add [1 2] [2 3])))  
      (is 
        (=
          [3 5 7]
          (vector/add [1 2 3] [2 3 4])))))

(deftest subtract
    (testing 
      "subtract"
      (is 
        (=
          [1 1]
          (vector/subtract [2 3] [1 2])))  
      (is 
        (=
          [1 1 1]
          (vector/subtract [2 3 4] [1 2 3])))))

(deftest magnitude
    (testing 
      "magnitude"
      (is 
        (= 5
           (vector/magnitude [4 3])))))

(deftest normalize
    (testing 
      "normalize"
      (is 
        (= [3/5 4/5]
           (vector/normalize [3 4])))))

(deftest multiply
    (testing 
      "multiply"
      (is 
        (= [2 4 8]
           (vector/multiply [1 2 4] 2)))))

(deftest divide
    (testing 
      "divide"
      (is 
        (= [1 2 4]
           (vector/divide [2 4 8] 2)))))

(deftest limit
    (testing 
      "limit"
      (is 
        (= [3 4]
           (vector/limit [6 8] 5)))))

(deftest set-magnitude
    (testing 
      "set-magnitude"
      (is 
        (= [3 4]
           (vector/set-magnitude [1 4/3] 5)))))

(deftest random-2d
    (testing 
      "random-2d"
      (let [points (repeatedly 10 vector/random-2d)]
        (is 
          (count points)
          (count (distinct points)))        
        (is 
          (every? #(close-to 1 (vector/magnitude %)) points)))))