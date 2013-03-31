(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core-test
  (:use clojure.test)
  (:require [quil.core :as q]
            [nature-of-code.test-utils :as test-utils]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.core :as smart-rockets])
  (:import [nature_of_code.genetic_algorithms.smart_rockets_superbasic.core DNA Rocket Population]))

(def test-params 
  {:size [300 400]
   :lifetime 100
   :mutation-rate 0.01
   :max-force 1.0 
   :target-r 20
   :rocket-count 3
   :rocket-r 4}) 

(defn gen-test-world [] 
  (let [dna (smart-rockets/random-dna 3)
        rocket1 (smart-rockets/gen-rocket :dna dna :fitness 1)
        rocket2 (smart-rockets/gen-rocket :dna dna :fitness 2)
        population (smart-rockets/gen-population :rockets (vector rocket1 rocket2))]
    (smart-rockets/gen-world :population population :target [100 100])))

(deftest test-dna
  (with-redefs [smart-rockets/params test-params] 
    (testing 
      "crossover"
      (is 
        (= 
          [1 2 3 :d :e]
          (:genes
            (with-redefs [rand-int (constantly 3)] ; crossover uses rand-int
              (smart-rockets/crossover (smart-rockets/gen-dna :genes [1 2 3 4 5]) (smart-rockets/gen-dna :genes [:a :b :c :d :e]))))))
      (is 
        (= 
          [:a :b :c :d :e]
          (:genes
            (with-redefs [rand-int (constantly 0)] ; crossover uses rand-int
              (smart-rockets/crossover (smart-rockets/gen-dna :genes [1 2 3 4 5]) (smart-rockets/gen-dna :genes [:a :b :c :d :e]))))))
      (is 
        (= 
          [1 2 3 4 5]
          (:genes
            (with-redefs [rand-int (constantly 5)] ; crossover uses rand-int
              (smart-rockets/crossover (smart-rockets/gen-dna :genes [1 2 3 4 5]) (smart-rockets/gen-dna :genes [:a :b :c :d :e])))))))
    (testing 
      "mutate"
      (let [random-genes (vector [0.0 0.0])]
        (is 
          (= 
            random-genes
            (:genes
              (smart-rockets/mutate (smart-rockets/gen-dna :genes random-genes :maxforce 0.1) 0.0))))) ; mutation-rate zero
      (let [random-genes (vector [0.0 0.0])]
        (is 
          (not= 
            random-genes
            (:genes
              (smart-rockets/mutate (smart-rockets/gen-dna :genes random-genes :maxforce 0.1) 1.0)))))) ; mutation-rate 100%
    (testing 
      "random-dna"
      (is
        (= 
          100
          (with-redefs [smart-rockets/params {:max-force 0.1}] ; depends on (params :max-force)
            (count (:genes (smart-rockets/random-dna 100)))))))))

(deftest test-rocket
  (with-redefs [smart-rockets/params test-params] 
    (testing 
      "next-motion-state"
      (is 
        (= 
          [1.1 1.2] 
          (:location
            (smart-rockets/next-motion-state (smart-rockets/gen-rocket :location [1 1] :velocity [0.1 0.2]))))))
    (testing 
      "move"
      (is 
        (not= [0 0] ; velocity ungleich null-vector
              (:velocity
                (smart-rockets/move (smart-rockets/gen-rocket :velocity [0 0] :acceleration [0 0] :dna (smart-rockets/random-dna 3)))))))
    (testing 
      "apply-force"
      (is 
        (=  
          [0.1 0.2]
          (:acceleration
            (smart-rockets/apply-force (smart-rockets/gen-rocket :mass 1.0 :acceleration [0 0]) [0.1 0.2])))))
    (is 
      (=  
        (smart-rockets/gen-rocket :mass 1.0 :acceleration [0.1 0.2])
        (smart-rockets/apply-force (smart-rockets/gen-rocket :mass 1.0 :acceleration [0 0]) [0.1 0.2])))
    (testing 
      "check-target"     
      (with-redefs [smart-rockets/params {:target-r 50}] ; depends on (params :target-r)
        (is 
          (=  
            true 
            (:hit-target
              (smart-rockets/check-target (smart-rockets/gen-rocket :location [100 100] :hit-target false) [100 100]))))
        (is 
          (=  
            false 
            (:hit-target 
              (smart-rockets/check-target (smart-rockets/gen-rocket :location [200 200] :hit-target true) [100 100]))))))
    (testing 
      "fitness" 
      (is 
        (test-utils/close-to 
          0.04
          (:fitness
            (smart-rockets/fitness (smart-rockets/gen-rocket :location [100 100] :fitness 0) [103 104])))))))

(deftest test-population
  (with-redefs [smart-rockets/params test-params] 
    (testing 
      "next-motion-state"
      (is 
        (not= [0 0] ; velocity ungleich null-vector
              (get-in
                (let [test-world (gen-test-world)]
                  (smart-rockets/move-and-check-rockets (:population test-world) (:target test-world)))
                [:rockets 0 :velocity]))))
    (testing 
      "calc-fitness"
      (is 
        (pos? ; greater than zero
              (get-in
                (let [test-world (gen-test-world)]
                  (smart-rockets/calc-rocket-fitness (:population test-world) (:target test-world)))
                [:rockets 0 :fitness]))))
    (testing 
      "populate-mating-pool"
      (is
        (=
         150 ; fitness 1 und 2
         (count 
           (:mating-pool 
             (let [test-world (gen-test-world)]
               (smart-rockets/populate-mating-pool (:population test-world))))))))
    (testing 
      "next-generation"
      (is
        (=
         2 ; hmm ... schwacher test
         (count
           (:rockets
             (let [test-world (gen-test-world)]
               (smart-rockets/next-generation 
                 (smart-rockets/populate-mating-pool (:population test-world)))))))))))
