(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core-test
  (:use 
    clojure.test)
  (:require 
    [quil.core :as q]
    [conjure.core :as conjure]
    [nature-of-code.test-utils :as test-utils]
    [nature-of-code.genetic-algorithms.smart-rockets-superbasic.core :as sr])
  (:import 
    [processing.core PVector]
    [nature_of_code.genetic_algorithms.smart_rockets_superbasic.core DNA Rocket]))

(defn gen-dna 
  [& {:keys [maxforce genes] 
      :or {maxforce 0.0 genes []}}] 
  (DNA. maxforce genes)) 

(defn gen-rocket
  [& {:keys [id mass location velocity acceleration r fitness dna gene-counter hit-target] 
      :or {id "rx" mass 0.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) 
           r 0 fitness 0 dna (gen-dna) gene-counter 0 hit-target false}}] 
    (Rocket. id mass location velocity acceleration r fitness dna gene-counter hit-target))

(deftest test-dna
  (testing 
    "crossover"
    (is 
      (= 
        [1 2 3 :d :e]
        (:genes
          (conjure/stubbing [rand-int 3] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e]))))))
    (is 
      (= 
        [:a :b :c :d :e]
        (:genes
          (conjure/stubbing [rand-int 0] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e]))))))
    (is 
      (= 
        [1 2 3 4 5]
        (:genes
          (conjure/stubbing [rand-int 5] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e])))))))
  (testing 
    "mutate"
    (let [random-genes (vector (PVector. 0.0 0.0))]
      (is 
        (= 
          random-genes
          (:genes
            (sr/mutate (gen-dna :genes random-genes :maxforce 0.1) 0.0))))) ; mutation-rate zero
    (let [random-genes (vector (PVector. 0.0 0.0))]
      (is 
        (not= 
          random-genes
          (:genes
            (sr/mutate (gen-dna :genes random-genes :maxforce 0.1) 1.0)))))) ; mutation-rate 100%
  (testing 
    "random-DNA"
    (is
      (= 
        100
        (count (:genes (sr/random-DNA 100)))))))

(deftest test-rocket
  (testing 
    "next-state"
    (is 
      (= 
        (PVector. 1.1 1.2) 
        (:location
          (sr/next-state (gen-rocket :location (PVector. 1 1) :velocity (PVector. 0.1 0.2)))))))
  (testing 
    "apply-force"
    (is 
      (=  
        (PVector. 0.1 0.2) 
        (:acceleration
          (sr/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
    (is 
      (=  
        (gen-rocket :mass 1.0 :acceleration (PVector. 0.1 0.2))
        (sr/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2)))))
  (testing 
    "check-target" ; der Test ist abhängig von (params :target-r)
    (is 
      (=  
        true 
        (:hit-target
          (sr/check-target (gen-rocket :location (PVector. 100 100)) (PVector. 100 100)))))
    (is 
      (=  
        false 
        (:hit-target 
          (sr/check-target (gen-rocket :location (PVector. 200 200)) (PVector. 100 100))))))
  (testing 
    "fitness" 
    (is 
      (test-utils/close-to 
        0.04
        (:fitness
            (sr/fitness (gen-rocket :location (PVector. 100 100)) (PVector. 103 104)))))))
