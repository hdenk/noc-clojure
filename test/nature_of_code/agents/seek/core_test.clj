(ns nature-of-code.agents.seek.core-test
  (:use clojure.test)
  (:require 
    [quil.core :as q]
    [nature-of-code.agents.seek.core :as seek-agent])
  (:import 
    [processing.core PVector]
    [nature_of_code.agents.seek.core Vehicle]))

(defn gen-vehicle
  [& {:keys [id mass location velocity acceleration r max-speed max-force] 
      :or {id "vx" mass 0.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) r 6 max-speed 0 max-force 0}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force)) 

(deftest test-particle
  (with-redefs [seek-agent/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (is 
        (= 
          (PVector. 1.1 1.2) 
          (:location 
            (seek-agent/move (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2))))))
      (is
        (= 
          (PVector. 0.1 0.2)  
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.1 0.2) :max-speed 1.0)))))
      (is
        (= 
          (PVector. 0.0 0.1) 
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.0 0.3) :max-speed 0.1)))))
      (is
        (= 
          (PVector. 0.1 0.0)  
          (:velocity 
            (seek-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.3 0.0) :max-speed 0.1)))))
      (is
        (= 
          (PVector. 0 0) 
          (:acceleration 
            (seek-agent/move (gen-vehicle :acceleration (PVector. 0.1 0.2))))))
      (is 
        (=  
          (gen-vehicle :location (PVector. 1.1 1.2) :velocity (PVector. 0.2 0.4) :acceleration (PVector. 0.0 0.0) :max-speed 1.0)
          (seek-agent/move (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2) :acceleration (PVector. 0.1 0.2) :max-speed 1.0)))))
    (testing 
      "apply-force"
      (is 
        (=  
          (PVector. 0.1 0.2) 
          (:acceleration 
            (seek-agent/apply-force (gen-vehicle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
      (is 
        (=  
          (gen-vehicle :mass 1.0 :acceleration (PVector. 0.1 0.2))
          (seek-agent/apply-force (gen-vehicle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
    (testing 
      "seek"
      (is 
        (=  
          (PVector. 0.9 0.0) 
          (:acceleration 
            (seek-agent/seek (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 2.0) (PVector. 10 0)))))
        (is 
          (=  
            (PVector. 0.1 0.0) 
            (:acceleration 
              (seek-agent/seek (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 0.1) (PVector. 10 0))))))))
