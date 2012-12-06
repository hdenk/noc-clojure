(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core-test
  (:use 
    clojure.test
    conjure.core)
  (:require 
    [quil.core :as q]
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
        {:genes [1 2 3 :d :e]}
        (select-keys
          (stubbing [rand-int 3] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e])))
          [:genes])))
    (is 
      (= 
        {:genes [:a :b :c :d :e]}
        (select-keys
          (stubbing [rand-int 0] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e])))
          [:genes])))
    (is 
      (= 
        {:genes [1 2 3 4 5]}
        (select-keys
          (stubbing [rand-int 5] ; crossover uses rand-int
            (sr/crossover (gen-dna :genes [1 2 3 4 5]) (gen-dna :genes [:a :b :c :d :e])))
          [:genes]))))
  (testing 
    "mutate"
    (let [random-genes (vector (PVector. 0.0 0.0))]
      (is 
        (= 
          {:genes random-genes}
          (select-keys
            (sr/mutate (gen-dna :genes random-genes :maxforce 0.1) 0.0) ; mutation-rate zero
            [:genes]))))
    (let [random-genes (vector (PVector. 0.0 0.0))]
      (is 
        (not= 
          {:genes random-genes}
          (select-keys
            (sr/mutate (gen-dna :genes random-genes :maxforce 0.1) 1.0) ; mutation-rate 100%
            [:genes])))))
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
        {:location (PVector. 1.1 1.2)} 
        (select-keys 
          (sr/next-state (gen-rocket :location (PVector. 1 1) :velocity (PVector. 0.1 0.2)))
          [:location]))))
  (testing 
    "apply-force"
    (is 
      (=  
        {:acceleration (PVector. 0.1 0.2)} 
        (select-keys 
          (sr/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:acceleration])))
    (is 
      (=  
        (select-keys 
          (gen-rocket :mass 1.0 :acceleration (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :r :fitness :dna :gene-counter :hit-target ])         
        (select-keys 
          (sr/apply-force (gen-rocket :mass 1.0 :acceleration (PVector. 0 0)) (PVector. 0.1 0.2))
          [:id :mass :location :velocity :acceleration :r :fitness :dna :gene-counter :hit-target])))))
