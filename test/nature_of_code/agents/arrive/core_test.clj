(ns nature-of-code.agents.arrive.core-test
  (:use clojure.test)
  (:require 
    [quil.core :as q]
    [nature-of-code.agents.arrive.core :as arrive-agent])
  (:import 
    [processing.core PVector]
    [nature_of_code.agents.arrive.core Vehicle]))

(defn gen-vehicle
  [& {:keys [id mass location velocity acceleration r max-speed max-force] 
      :or {id "vx" mass 0.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) r 6 max-speed 0 max-force 0}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force)) 

(deftest test-particle
  (with-redefs [arrive-agent/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (is 
        (= 
          (PVector. 1.1 1.2)
          (:location 
            (arrive-agent/move (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2))))))
      (is
        (= 
          (PVector. 0.1 0.2)  
          (:velocity 
            (arrive-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.1 0.2) :max-speed 1.0)))))
      (is
        (= 
          (PVector. 0.0 0.1)  
          (:velocity 
            (arrive-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.0 0.3) :max-speed 0.1)))))
      (is
        (= 
          (PVector. 0.1 0.0)  
          (:velocity 
            (arrive-agent/move (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.3 0.0) :max-speed 0.1)))))
      (is
        (= 
          (PVector. 0 0)  
          (:acceleration 
            (arrive-agent/move (gen-vehicle :acceleration (PVector. 0.1 0.2))))))
      (is 
        (=  
          (gen-vehicle :location (PVector. 1.1 1.2) :velocity (PVector. 0.2 0.4) :acceleration (PVector. 0.0 0.0) :max-speed 1.0)
          (arrive-agent/move (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2) :acceleration (PVector. 0.1 0.2) :max-speed 1.0)))))
    (testing 
      "apply-force"
      (is 
        (=  
          (PVector. 0.1 0.2) 
          (:acceleration 
            (arrive-agent/apply-force (gen-vehicle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
      (is 
        (=  
          (gen-vehicle :mass 1.0 :acceleration (PVector. 0.1 0.2))
          (arrive-agent/apply-force (gen-vehicle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
    (testing 
      "arrive" ; diese Tests sind leider abhängig von (params :arrive-r) = 50
      (with-redefs [arrive-agent/params {:arrive-r 50}] ; dependent on (params :arrive-r)
        (is 
          (=  
            (PVector. 0.9 0.0) 
            (:acceleration 
              (arrive-agent/arrive (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 2.0) (PVector. 100 0)))))
        (is 
          (=  
            (PVector. 0.1 0.0) 
            (:acceleration 
              (arrive-agent/arrive (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 0.1) (PVector. 100 0)))))
        (is 
          (=  
            (PVector. -0.1 0.0) 
            (:acceleration 
              (arrive-agent/arrive (gen-vehicle :mass 1.0 :location (PVector. 90 0) :velocity (PVector. 1.0 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 0.1) (PVector. 100 0)))))))))
