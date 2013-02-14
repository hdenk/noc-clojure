(ns nature-of-code.forces.fluidresistance.core-test
  (:use clojure.test)
  (:require [quil.core :as qc]
            [nature-of-code.test-utils :as test-utils]
            [nature-of-code.forces.fluidresistance.core :as fluid-resistance])
  (:import [nature_of_code.forces.fluidresistance.core Mover Fluid]))

(defn gen-fluid 
  [& {:keys [id x y width height color drag-coefficient] 
      :or {id "fx" x 0 y 0 width 0 height 0 color 0 drag-coefficient 0}}] 
  (Fluid. id x y width height color drag-coefficient)) 

(defn gen-mover 
  [& {:keys [id mass location velocity acceleration color] 
      :or {id "mx" mass 0 location [0 0] velocity [0 0] acceleration [0 0] color 0}}] 
  (Mover. id mass location velocity acceleration color)) 

(deftest fluid
  (with-redefs [fluid-resistance/params {}] ; to avoid unintended dependencies in params
    (testing 
      "contains-mover?"
      (is 
        (= 
          false
           (let [fluid (gen-fluid :width 600 :height 400)
                 mover (gen-mover :location [-1 -1])]
             (fluid-resistance/contains-mover? fluid mover))))
      (is 
        (= 
          true
           (let [fluid (gen-fluid :width 600 :height 400)
                 mover (gen-mover :location [0 0])]
             (fluid-resistance/contains-mover? fluid mover))))
      (is 
        (= 
          true
           (let [fluid (gen-fluid :width 600 :height 400)
                 mover (gen-mover :location [300 200])]
             (fluid-resistance/contains-mover? fluid mover))))
      (is 
        (= 
          true
           (let [fluid (gen-fluid :width 600 :height 400)
                 mover (gen-mover :location [600 400])]
             (fluid-resistance/contains-mover? fluid mover))))
      (is 
        (= 
          false
           (let [fluid (gen-fluid :width 600 :height 400)
                 mover (gen-mover :location [601 401])]
             (fluid-resistance/contains-mover? fluid mover)))))
    (testing 
      "drag-force"
      (is 
        (= 
          [-20.0 -15.0] ; Wurzel aus (20*20)+(15*15)=25
           (let [fluid (gen-fluid :width 600 :height 400 :drag-coefficient 1.0)
                 mover (gen-mover :location [200 100] :velocity [4 3])]
             (fluid-resistance/drag-force fluid mover))))
      (is 
        (= 
          [0 0] ; not in fluid -> no force
           (let [fluid (gen-fluid :width 600 :height 400 :drag-coefficient 1.0)
                 mover (gen-mover :location [-1 -1] :velocity [4 3])]
             (fluid-resistance/drag-force fluid mover)))))))

(deftest mover
  (with-redefs [fluid-resistance/params {:r-factor 1} ; check-edges depends on (params :r-factor)
                qc/height (constantly 400)] ; and an gc/height 
    (testing 
      "update-mover"
      (is 
        (= 
         [301 202]
         (:location
	         (let [fluid (gen-fluid :width 600 :height 400 :drag-coefficient 1.0)
	              mover (gen-mover :mass 1 :location [300 200] :velocity [1 2])]
	           (fluid-resistance/update-mover mover fluid))))))))