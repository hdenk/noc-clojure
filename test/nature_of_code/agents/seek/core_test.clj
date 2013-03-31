(ns nature-of-code.agents.seek.core-test
  (:use clojure.test)
  (:require [quil.core :as q]
            [nature-of-code.agents.seek.core :as seek-agent])
  (:import [nature_of_code.agents.seek.core Vehicle]))

(defn gen-vehicle
  [& {:keys [id mass location velocity acceleration r max-speed max-force] 
      :or {id "vx" mass 0.0 location [0 0] velocity [0 0] acceleration [0 0] r 6 max-speed 0 max-force 0}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force)) 

(deftest test-particle
  (with-redefs [seek-agent/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (is 
        (= 
          [1.1 1.2] 
          (:location 
            (seek-agent/move (gen-vehicle :location [1 1] :velocity [0.1 0.2])))))
      (is
        (= 
          [0.1 0.2]  
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity [0.0 0.0] :acceleration [0.1 0.2] :max-speed 1.0)))))
      (is
        (= 
          [0.0 0.1] 
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity [0.0 0.0] :acceleration [0.0 0.3] :max-speed 0.1)))))
      (is
        (= 
          [0.1 0.0]  
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity [0.0 0.0] :acceleration [0.3 0.0] :max-speed 0.1)))))
      (is
        (= 
          [0.0 0.0] 
          (:acceleration 
            (seek-agent/move (gen-vehicle :acceleration [0.1 0.2])))))
      (is 
        (=  
          (gen-vehicle :location [1.1 1.2] :velocity [0.2 0.4] :acceleration [0.0 0.0] :max-speed 1.0)
          (seek-agent/move (gen-vehicle :location [1 1] :velocity [0.1 0.2] :acceleration [0.1 0.2] :max-speed 1.0)))))
    (testing 
      "apply-force"
      (is 
        (=  
          [0.1 0.2]
          (:acceleration 
            (seek-agent/apply-force (gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
      (is 
        (=  
          (gen-vehicle :mass 1.0 :acceleration [0.1 0.2])
          (seek-agent/apply-force (gen-vehicle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
    (testing 
      "seek"
      (is 
        (=  
          [0.9 0.0] 
          (:acceleration 
            (seek-agent/seek (gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 2.0) [10 0]))))
      (is 
        (=  
          [0.1 0.0]
          (:acceleration 
            (seek-agent/seek (gen-vehicle :mass 1.0 :location [0 0] :velocity [0.1 0] :acceleration [0 0] :max-speed 1.0 :max-force 0.1) [10 0])))))))
