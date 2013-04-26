(ns nature-of-code.neural-networks.neural-seek.core-test
  (:use clojure.test)
  (:require [nature-of-code.neural-networks.neural-seek.core :as neural-seek])
  (:import [nature_of_code.neural_networks.neural_seek.core Perceptron]
           [nature_of_code.neural_networks.neural_seek.core Vehicle]))

(deftest test-perceptron
  (with-redefs [neural-seek/params {}] ; to avoid unintended dependencies in params
    (testing 
      "feed-forward"
      (is 
        (= 
          [0.5 0.5] 
          (let [forces [[1.0 0.0] [0.0 1.0] [-1.0 -1.0]]
                weights [1.0 1.0 0.5]]
            (neural-seek/feed-forward (neural-seek/gen-perceptron :weights weights) forces)))))
    (testing 
      "train"
      (is 
        (= 
          [0.51 0.47 0.97] 
          (:weights (let [forces [[1.0 0.0] [1.0 -2.0] [-1.0 -1.0]]
                weights [0.5 0.5 1.0]
                learning-rate 0.001
                error [10.0 20.0]]
            (neural-seek/train (neural-seek/gen-perceptron :weights weights :learning-rate learning-rate) forces error))))))))

(deftest test-vehicle
  (with-redefs [neural-seek/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (is 
        (= 
          [1.1 1.2] 
          (:location 
            (neural-seek/move (neural-seek/gen-vehicle :location [1 1] :velocity [0.1 0.2])))))
      (is
        (= 
          [0.1 0.2]  
          (:velocity 
            (neural-seek/move (neural-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.1 0.2] :max-speed 1.0)))))
      (is
        (= 
          [0.0 0.1] 
          (:velocity 
            (neural-seek/move (neural-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.0 0.3] :max-speed 0.1)))))
      (is
        (= 
          [0.1 0.0]  
          (:velocity 
            (neural-seek/move (neural-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.3 0.0] :max-speed 0.1)))))
      (is
        (= 
          [0.0 0.0] 
          (:acceleration 
            (neural-seek/move (neural-seek/gen-vehicle :acceleration [0.1 0.2])))))
      (is 
        (=  
          (neural-seek/gen-vehicle :location [1.1 1.2] :velocity [0.2 0.4] :acceleration [0.0 0.0] :max-speed 1.0)
          (neural-seek/move (neural-seek/gen-vehicle :location [1 1] :velocity [0.1 0.2] :acceleration [0.1 0.2] :max-speed 1.0)))))
    (testing 
      "apply-force"
      (is 
        (=  
          [0.1 0.2]
          (:acceleration 
            (neural-seek/apply-force (neural-seek/gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
      (is
        (=  
          (neural-seek/gen-vehicle :mass 1.0 :acceleration [0.1 0.2])
          (neural-seek/apply-force (neural-seek/gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
    (testing 
      "seek"
      (is 
        (=  
          [0.9 0.0] 
          (neural-seek/seek (neural-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 2.0) [10 0])))
      (is 
        (=  
          [0.1 0.0]
          (neural-seek/seek (neural-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 0.1) [10 0]))))
    (testing 
      "steer"
      (is 
        (=  
          [1.0 1.0] 
          (:acceleration (let [weights [1.0 1.0 1.0]
                learning-rate 0.001
                perceptron (neural-seek/gen-perceptron :weights weights :learning-rate learning-rate)
                vehicle (neural-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 2.0 :perceptron perceptron)
                targets [[0 100][100 0]]
                desired-location [100 100]]
            (neural-seek/steer vehicle targets desired-location))))))))