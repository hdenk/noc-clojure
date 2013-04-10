(ns nature-of-code.oscillation.oscillating-objects.core-test
  (:use clojure.test)
  (:require [nature-of-code.oscillation.oscillating-objects.core :as oscillating-objects])
  (:import [nature_of_code.oscillation.oscillating_objects.core Oscillator]))

(defn gen-oscillator 
  [& {:keys [id angle velocity amplitude] 
      :or {id "ox" angle [0 0] velocity [0 0] amplitude [0 0]}}] 
  (Oscillator. id angle velocity amplitude)) 

(deftest test-particle
  (with-redefs [oscillating-objects/params {}] ; to avoid unintended dependencies in params
    (testing 
      "move"
      (is 
        (= 
          [0.1 0.2] 
          (:angle 
            (oscillating-objects/move (gen-oscillator :angle [0 0] :velocity [0.1 0.2]))))))))
