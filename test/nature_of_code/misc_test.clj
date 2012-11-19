(ns nature-of-code.core-test
  (:use clojure.test nature-of-code.test-utils)
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(deftest test-normalize
  (testing "test PVector normalize"
    (let [v (PVector. 10 0 )]
      (.normalize v) ; Seiteneffekt !
      (is (pvector-close-to (PVector. 1.0 0) v)))))
