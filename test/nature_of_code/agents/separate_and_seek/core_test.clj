(ns nature-of-code.agents.separate-and-seek.core-test
  (:use clojure.test)
  (:require [quil.core :as q]
            [nature-of-code.agents.separate-and-seek.core :as separate-and-seek])
  (:import [nature_of_code.agents.separate_and_seek.core Vehicle]))

(deftest test-vehicle
  (with-redefs [separate-and-seek/params {}] ; to avoid unintended dependencies in params
    (testing 
      "init"
      (is 
        (= 
          3
          (with-redefs [separate-and-seek/params {:vehicle-r 6 :max-force 0.1 :max-speed 5}] ; references vehicle-r
            (let [sketch-size [800 600]]
              (count (separate-and-seek/gen-and-init-vehicles 3 sketch-size)))))))
    (testing 
      "move"
      (is 
        (= 
          [1.1 1.2] 
          (:location 
            (separate-and-seek/move (separate-and-seek/gen-vehicle :location [1 1] :velocity [0.1 0.2])))))
      (is
        (= 
          [0.1 0.2]  
          (:velocity 
            (separate-and-seek/move (separate-and-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.1 0.2] :max-speed 1.0)))))
      (is
        (= 
          [0.0 0.1] 
          (:velocity 
            (separate-and-seek/move (separate-and-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.0 0.3] :max-speed 0.1)))))
      (is
        (= 
          [0.1 0.0]  
          (:velocity 
            (separate-and-seek/move (separate-and-seek/gen-vehicle :velocity [0.0 0.0] :acceleration [0.3 0.0] :max-speed 0.1)))))
      (is
        (= 
          [0.0 0.0] 
          (:acceleration 
            (separate-and-seek/move (separate-and-seek/gen-vehicle :acceleration [0.1 0.2])))))
      (is 
        (=  
          (separate-and-seek/gen-vehicle :location [1.1 1.2] :velocity [0.2 0.4] :acceleration [0.0 0.0] :max-speed 1.0)
          (separate-and-seek/move (separate-and-seek/gen-vehicle :location [1 1] :velocity [0.1 0.2] :acceleration [0.1 0.2] :max-speed 1.0)))))
    (testing 
      "apply-force"
      (is 
        (=  
          [0.1 0.2]
          (:acceleration 
            (separate-and-seek/apply-force (separate-and-seek/gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
      (is 
        (=  
          (separate-and-seek/gen-vehicle :mass 1.0 :acceleration [0.1 0.2])
          (separate-and-seek/apply-force (separate-and-seek/gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
    (testing 
      "seek"
      (is 
        (=  
          [0.9 0.0] 
          (let [vehicle (separate-and-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 2.0)
                target [10 0]]
            (separate-and-seek/seek vehicle target))))
      (is 
        (=  
          [0.1 0.0]
          (let [vehicle (separate-and-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 0.1)
                target [10 0]]
          (separate-and-seek/seek vehicle target)))))
    (testing 
      "separate"
      (is 
        (=  
          [0.9 0.0] 
          (with-redefs [separate-and-seek/params {:vehicle-r 6}] ; references vehicle-r
	          (let [vehicle (separate-and-seek/gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 2.0)
	                vehicle-r (separate-and-seek/params :vehicle-r)
	                other-vehicles [(assoc vehicle :location [vehicle-r 0]) (assoc vehicle :location [0 vehicle-r])]]
	            (separate-and-seek/separate vehicle other-vehicles))))))))
