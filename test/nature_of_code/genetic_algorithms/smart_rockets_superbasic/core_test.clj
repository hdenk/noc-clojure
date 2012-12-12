(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core-test
  (:use 
    clojure.test)
  (:require 
    [quil.core :as q]
    [nature-of-code.test-utils :as test-utils]
    [nature-of-code.genetic-algorithms.smart-rockets-superbasic.core :as smart-rockets])
  (:import 
    [processing.core PVector]
    [nature_of_code.genetic_algorithms.smart_rockets_superbasic.core DNA Rocket Population]))

(defn gen-dna 
  [& {:keys [maxforce genes] 
      :or {maxforce 0.0 genes []}}] 
  (DNA. maxforce genes)) 

(defn gen-rocket
  [& {:keys [id mass location velocity acceleration r fitness dna gene-counter hit-target] 
      :or {id "rx" mass 0.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) 
           r 0 fitness 0 dna (gen-dna) gene-counter 0 hit-target false}}] 
  (Rocket. id mass location velocity acceleration r fitness dna gene-counter hit-target))

(defn gen-population 
  [& {:keys [mutation-rate rockets mating-pool generation-count] 
      :or {mutation-rate 0.0 rockets [] mating-pool [] generation-count 0}}] 
  (Population. mutation-rate rockets mating-pool generation-count)) 

(deftest test-dna
  (with-redefs [smart-rockets/params {}] 
    (testing 
      "crossover"
      (is 
        (= 
          [1 2 3 :d :e]
          (:genes
            (with-redefs [rand-int (constantly 3)] ; crossover uses rand-int
              (smart-rockets/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e]))))))
      (is 
        (= 
          [:a :b :c :d :e]
          (:genes
            (with-redefs [rand-int (constantly 0)] ; crossover uses rand-int
              (smart-rockets/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e]))))))
      (is 
        (= 
          [1 2 3 4 5]
          (:genes
            (with-redefs [rand-int (constantly 5)] ; crossover uses rand-int
              (smart-rockets/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e])))))))
    (testing 
      "mutate"
      (let [random-genes (vector (PVector. 0.0 0.0))]
        (is 
          (= 
            random-genes
            (:genes
              (smart-rockets/mutate (gen-dna :genes random-genes :maxforce 0.1) 0.0))))) ; mutation-rate zero
      (let [random-genes (vector (PVector. 0.0 0.0))]
        (is 
          (not= 
            random-genes
            (:genes
              (smart-rockets/mutate (gen-dna :genes random-genes :maxforce 0.1) 1.0)))))) ; mutation-rate 100%
    (testing 
      "random-dna"
      (is
        (= 
          100
          (with-redefs [smart-rockets/params {:max-force 0.1}] ; depends on (params :max-force)
            (count (:genes (smart-rockets/random-dna 100)))))))))

(deftest test-rocket
  (with-redefs [smart-rockets/params {}] 
    (testing 
      "move"
      (is 
        (= 
          (PVector. 1.1 1.2) 
          (:location
            (smart-rockets/move (gen-rocket :location (PVector. 1 1) :velocity (PVector. 0.1 0.2)))))))
    (testing 
      "apply-force"
      (is 
        (=  
          (PVector. 0.1 0.2) 
          (:acceleration
            (smart-rockets/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
      (is 
        (=  
          (gen-rocket :mass 1.0 :acceleration (PVector. 0.1 0.2))
          (smart-rockets/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
    (testing 
      "check-target"     
      (with-redefs [smart-rockets/params {:target-r 50}] ; depends on (params :target-r)
        (is 
          (=  
            true 
            (:hit-target
              (smart-rockets/check-target (gen-rocket :location (PVector. 100 100)) (PVector. 100 100)))))
        (is 
          (=  
            false 
            (:hit-target 
              (smart-rockets/check-target (gen-rocket :location (PVector. 200 200)) (PVector. 100 100)))))))
    (testing 
      "fitness" 
      (is 
        (test-utils/close-to 
          0.04
          (:fitness
            (smart-rockets/fitness (gen-rocket :location (PVector. 100 100)) (PVector. 103 104))))))))

(deftest test-population
  (with-redefs [smart-rockets/params {}] 
    (testing 
      "next-fitness"
      (is 
        (test-utils/close-to
          0.04
          (:fitness
            (first
              (:rockets
                (smart-rockets/calc-fitness 
                  (gen-population :rockets (vector (gen-rocket))) (PVector. 3 4)))))))) 
    (testing 
      "next-mating-pool"
      (is
         (=
           133
           (count 
             (:mating-pool 
               (smart-rockets/populate-mating-pool 
                 (gen-population :rockets (vector (gen-rocket :fitness 0.1) (gen-rocket :fitness 0.3)))))))))
    (testing 
      "next-generation"
      (is
         (=
           2
           (count (:rockets
             (with-redefs [smart-rockets/params {:max-force 0.1 :size [400 600]}] ; depends on (params :max-force) and (params :size)
               (smart-rockets/next-generation 
                 (smart-rockets/populate-mating-pool 
                   (gen-population :rockets (vector (gen-rocket :fitness 0.1) (gen-rocket :fitness 0.3)))))))))))))

