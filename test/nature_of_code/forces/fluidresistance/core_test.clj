(ns nature-of-code.forces.fluidresistance.core-test
  (:use clojure.test)
  (:require 
    [quil.core :as q]
    [nature-of-code.test-utils :as test-utils]
    [nature-of-code.forces.fluidresistance.core :as fluid-resistance])
  (:import 
    [processing.core PVector]
    [nature_of_code.forces.fluidresistance.core Mover Fluid]))

(deftest test-mover
  (testing 
    "next-state"
    (is 
      (= (Mover. "m1" 2.0 (PVector. 1 1.1) (PVector. 0.1 0.1) (PVector. 0 0) 127)
         (let [m (Mover. "m1" 2.0 (PVector. 1 1) (PVector. 0.0 0.1) (PVector. 0.1 0) 127)]
           (fluid-resistance/next-state m)))))
  (testing 
    "apply-force"
    (is 
      (= (Mover. "m1" 2.0 (PVector. 1 1) (PVector. 0.0 0.1) (PVector. 0.1 0.1) 127)
         (let [m (Mover. "m1" 2.0 (PVector. 1 1) (PVector. 0.0 0.1) (PVector. 0 0.1) 127)]
           (fluid-resistance/apply-force m (PVector. 0.2 0))))))
  (testing 
    "check-edges"
    (with-redefs-fn 
      {#'q/height (constantly 400)}       
      #(is 
         (= (Mover. "m1" 2.0 (PVector. 600 400) (PVector. -0.9 -0.9) (PVector. 0 0) 127)
            (let [m (Mover. "m1" 2.0 (PVector. 600 411) (PVector. 1 1) (PVector. 0 0) 127)]
              (fluid-resistance/check-edges m)))))))

(deftest test-fluid
  (testing 
    "contains-mover?"
    (is 
      (= false
         (let [f (Fluid. "f1" 0 0 600 400 127 1.0)
               m (Mover. "m1" 2.0 (PVector. 0 0) (PVector. 0.0 0.1) (PVector. 0 0.1) 127)]
           (fluid-resistance/contains-mover? f m))))
    (is 
      (= true
         (let [f (Fluid. "f1" 0 0  600 400 127 1.0)
               m (Mover. "m1" 2.0 (PVector. 1 1) (PVector. 0.0 0.1) (PVector. 0 0.1) 127)]
           (fluid-resistance/contains-mover? f m))))
    (is 
      (= true
         (let [f (Fluid. "f1" 0 0 600 400 127 1.0)
               m (Mover. "m1" 2.0 (PVector. 599 399) (PVector. 0.0 0.1) (PVector. 0 0.1) 127)]
           (fluid-resistance/contains-mover? f m))))
    (is 
      (= false
         (let [f (Fluid. "f1" 0 0 600 400 127 1.0)
               m (Mover. "m1" 2.0 (PVector. 600 400) (PVector. 0.0 0.1) (PVector. 0 0.1) 127)]
           (fluid-resistance/contains-mover? f m)))))
  (testing 
    "drag-force"
    (is 
      (test-utils/pvector-close-to 
        (PVector. -20 -15) ; Wurzel aus (20*20)+(15*15)=25
        (let [f (Fluid. "f1" 0 0  600 400 127 1.0)
              m (Mover. "m1" 2.0 (PVector. 1 1) (PVector. 4 3) (PVector. 0 0) 127)]
          (fluid-resistance/drag-force f m))))))
