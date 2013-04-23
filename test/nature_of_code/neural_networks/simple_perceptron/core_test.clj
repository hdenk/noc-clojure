(ns nature-of-code.neural-networks.simple-perceptron.core-test
  (:use clojure.test)
  (:require [nature-of-code.neural-networks.simple-perceptron.core :as simple-perceptron])
  (:import [nature_of_code.neural_networks.simple_perceptron.core Perceptron]
           [nature_of_code.neural_networks.simple_perceptron.core TrainingRecord]))

(deftest test-training-record
  (with-redefs [simple-perceptron/params {:x-min -400 :x-max 400 :y-min -100 :y-max 100}] ; to avoid unintended dependencies in params
    (testing 
      "gen-training-data"
      (is 
        (let [training-data (simple-perceptron/gen-training-data 3)]
          (and (= (count training-data) 3) 
               (every? #(instance? TrainingRecord %) training-data)))))))            

(deftest test-perceptron
  (with-redefs [simple-perceptron/params {}] ; to avoid unintended dependencies in params
    (testing 
      "feed-forward"
      (is 
        (= 
          1 
          (let [inputs [1 1 1]
                weights [1 1 1]]
            (simple-perceptron/feed-forward (simple-perceptron/gen-perceptron :weights weights) inputs))))
      (is 
        (= 
          -1 
          (let [inputs [-1 -1 1]
                weights [1 1 1]]
            (simple-perceptron/feed-forward (simple-perceptron/gen-perceptron :weights weights) inputs))))
      (is 
        (= 
          1 
          (let [inputs [1 -1 1]
                weights [1 1 1]]
            (simple-perceptron/feed-forward (simple-perceptron/gen-perceptron :weights weights) inputs)))))
    (testing 
      "activate"
      (is 
        (= 
          1 
          (simple-perceptron/activate (simple-perceptron/gen-perceptron) 1)))
      (is 
        (= 
          -1 
          (simple-perceptron/activate (simple-perceptron/gen-perceptron) 0)))  
      (is 
        (= 
          -1 
          (simple-perceptron/activate (simple-perceptron/gen-perceptron) -1))))
    (testing 
      "train"
      (is 
        (= 
          [0 0 0] 
          (:weights (let [inputs [1 1 1]
                weights [1 1 1]
                learning-rate 0.001
                desired 1]
            (simple-perceptron/train (simple-perceptron/gen-perceptron :weights weights :learning-rate learning-rate) inputs desired)))))  
      (is 
        (= 
          [0 0 0] 
          (:weights (let [inputs [1 1 1]
                weights [1 1 1]
                learning-rate 0.001
                desired -1]
            (simple-perceptron/train (simple-perceptron/gen-perceptron :weights weights :learning-rate learning-rate) inputs desired))))))))  
