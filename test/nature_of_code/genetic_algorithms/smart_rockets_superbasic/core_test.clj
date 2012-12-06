(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core-test
  (:use 
    clojure.test
    conjure.core)
  (:require 
    [quil.core :as q]
    [nature-of-code.genetic-algorithms.smart-rockets-superbasic.core :as sr])
  (:import 
    [processing.core PVector]
    [nature_of_code.genetic_algorithms.smart_rockets_superbasic.core DNA]))

(defn gen-dna 
  [& {:keys [maxforce genes] 
      :or {maxforce 0.0 genes []}}] 
  (DNA. maxforce genes)) 

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
