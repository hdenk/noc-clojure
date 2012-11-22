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
      :or {id "vx" mass 1.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) r 6 max-speed 0 max-force 0}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force)) 

(deftest test-particle
  (testing 
    "next-state"
    (is 
      (= 
        {:location (PVector. 1.1 1.2)} 
        (select-keys 
          (arrive-agent/next-state (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2)))
          [:location])))
    (is
      (= 
        {:velocity (PVector. 0.1 0.2)}  
         (select-keys 
           (arrive-agent/next-state (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.1 0.2) :max-speed 1.0))
           [:velocity])))
    (is
      (= 
        {:velocity (PVector. 0.0 0.1)}  
         (select-keys 
           (arrive-agent/next-state (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.0 0.3) :max-speed 0.1))
           [:velocity])))
    (is
      (= 
        {:velocity (PVector. 0.1 0.0)}  
         (select-keys 
           (arrive-agent/next-state (gen-vehicle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.3 0.0) :max-speed 0.1))
           [:velocity])))
    (is
      (= 
        {:acceleration (PVector. 0 0)}  
         (select-keys 
           (arrive-agent/next-state (gen-vehicle :acceleration (PVector. 0.1 0.2)))
           [:acceleration])))
    (is 
      (=  
        (select-keys 
          (gen-vehicle :location (PVector. 1.1 1.2) :velocity (PVector. 0.2 0.4) :acceleration (PVector. 0.0 0.0) :max-speed 1.0)
          [:id :mass :location :velocity :acceleration :r :max-speed :max-force]) 
        (select-keys 
          (arrive-agent/next-state (gen-vehicle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2) :acceleration (PVector. 0.1 0.2) :max-speed 1.0))
          [:id :mass :location :velocity :acceleration :r :max-speed :max-force]))))
  (testing 
    "apply-force"
    (is 
      (=  
        {:acceleration (PVector. 0.1 0.2)} 
        (select-keys 
          (arrive-agent/apply-force (gen-vehicle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:acceleration])))
    (is 
      (=  
        (select-keys 
          (gen-vehicle :acceleration (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :r :max-speed :max-force])         
        (select-keys 
          (arrive-agent/apply-force (gen-vehicle :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :r :max-speed :max-force]))))
  (testing 
    "arrive"
    (is 
      (=  
        {:acceleration (PVector. 0.9 0.0)} 
        (select-keys 
          (arrive-agent/arrive (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 2.0) (PVector. 10 0))
          [:acceleration])))

    (is 
      (=  
        {:acceleration (PVector. 0.1 0.0)} 
        (select-keys 
          (arrive-agent/arrive (gen-vehicle :mass 1.0 :location (PVector. 0 0) :velocity (PVector. 0.1 0) :acceleration (PVector. 0 0) :max-speed 1.0 :max-force 0.1) (PVector. 10 0))
          [:acceleration])))))

