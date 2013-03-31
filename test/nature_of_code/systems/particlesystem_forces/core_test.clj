(ns nature-of-code.systems.particlesystem-forces.core-test
  (:use clojure.test)
  (:require [quil.core :as q]
            [nature-of-code.systems.particlesystem-forces.core :as ps])
  (:import [nature_of_code.systems.particlesystem_forces.core Particle]))

(defn gen-particle 
  [& {:keys [id mass location velocity acceleration lifespan] 
      :or {id "px" mass 1.0 location [0 0] velocity [0 0] acceleration [0 0] lifespan 0}}] 
  (Particle. id mass location velocity acceleration lifespan)) 

(deftest test-particle
  (with-redefs [ps/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (with-redefs [ps/params {:lifespan-dec-rate 2}] ; dependent on (params :lifespan-dec-rate)
        (is 
          (= 
            [1.1 1.2] 
            (:location 
              (ps/move (gen-particle :location [1 1] :velocity [0.1 0.2])))))
        (is
          (= 
            [0.1 0.2] 
            (:velocity 
              (ps/move (gen-particle :velocity [0.0 0.0] :acceleration [0.1 0.2])))))
        (is
          (= 
            [0.0 0.0] 
            (:acceleration 
              (ps/move (gen-particle :acceleration [0.1 0.2])))))
        (is
          (= 
            98.0  
            (:lifespan 
              (ps/move (gen-particle :lifespan 100.0)))))
        (is 
          (=  
            (gen-particle :location [1.1 1.2] :velocity [0.2 0.4] :acceleration [0.0 0.0] :lifespan 98.0)
            (ps/move (gen-particle :location [1 1] :velocity [0.1 0.2] :acceleration [0.1 0.2] :lifespan 100.0))))))
    (testing 
      "apply-force"
      (is 
        (=  
          [0.1 0.2] 
          (:acceleration 
            (ps/apply-force (gen-particle :mass 1.0 :acceleration [0 0]) [0.1 0.2]))))
      (is 
        (=  
          (gen-particle :acceleration [0.1 0.2])
          (ps/apply-force (gen-particle :acceleration [0 0]) [0.1 0.2]))))
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
          (ps/expired? (gen-particle :lifespan -1.0)))))))
