(ns nature-of-code.systems.particlesystem-polymorphism.core-test
  (:use clojure.test)
  (:require 
    [quil.core :as q]
    [nature-of-code.systems.particlesystem-polymorphism.core :as ps])
  (:import 
    [processing.core PVector]
    [nature_of_code.systems.particlesystem_polymorphism.core CircularConfetti]))

(defn gen-particle 
  [& {:keys [id mass location velocity acceleration lifespan] 
      :or {id "px" mass 1.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) lifespan 0}}] 
  (CircularConfetti. id mass location velocity acceleration lifespan)) 

(deftest test-particle
  (testing 
    "next-state"
    (is 
      (= 
        {:location (PVector. 1.1 1.2)} 
        (select-keys 
          (ps/next-state (gen-particle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2)))
          [:location])))
    (is
      (= 
        {:velocity (PVector. 0.1 0.2)}  
         (select-keys 
           (ps/next-state (gen-particle :velocity (PVector. 0.0 0.0) :acceleration (PVector. 0.1 0.2)))
           [:velocity])))
    (is
      (= 
        {:acceleration (PVector. 0 0)}  
         (select-keys 
           (ps/next-state (gen-particle :acceleration (PVector. 0.1 0.2)))
           [:acceleration])))
    (is
      (= 
         {:lifespan 98.0}  
         (select-keys 
           (ps/next-state (gen-particle :lifespan 100.0))
           [:lifespan])))
    (is 
      (=  
        (select-keys 
          (gen-particle :location (PVector. 1.1 1.2) :velocity (PVector. 0.2 0.4) :acceleration (PVector. 0.0 0.0) :lifespan 98.0)
          [:id :mass :location :velocity :acceleration :lifespan]) 
        (select-keys 
          (ps/next-state (gen-particle :location (PVector. 1 1) :velocity (PVector. 0.1 0.2) :acceleration (PVector. 0.1 0.2) :lifespan 100.0))
          [:id :mass :location :velocity :acceleration :lifespan]))))
  (testing 
    "apply-force"
    (is 
      (=  
        {:acceleration (PVector. 0.1 0.2)} 
        (select-keys 
          (ps/apply-force (gen-particle :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:acceleration])))
    (is 
      (=  
        (select-keys 
          (gen-particle :acceleration (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :lifespan])         
        (select-keys 
          (ps/apply-force (gen-particle :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :lifespan]))))
  (testing 
    "expired?"
    (is 
      (false?  
        (ps/expired? (gen-particle :lifespan 1.0))))
    (is 
      (false?
        (ps/expired? (gen-particle :lifespan 0.0))))
    (is 
      (true?
        (ps/expired? (gen-particle :lifespan -1.0))))))
