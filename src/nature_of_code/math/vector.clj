(ns nature-of-code.math.vector
  "A minimalistic Implementation of basic Vector-Math
   Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [clojure.math.numeric-tower :as math]))

(defn add [& vs]
  "vector addition"
  (vec (apply map + vs)))

(defn subtract [& vs]
  "vector subtraction"
  (vec (apply map - vs)))

(defn magnitude [v]
  "returns the magnitude of a vector"
  (math/sqrt (reduce + (map #(math/expt % 2) v))))

(defn normalize [v]
  "vector normalization"
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

(defn multiply [v scalar ]
  "vector multiplication"
  (vec (map * (repeat scalar) v)))

(defn divide [v scalar]
  "vector division"
  (vec (map / v (repeat scalar))))

(defn limit [v upper]
  "returns a vector constrained with an upper magnitude"
  (let [m (magnitude v)]
    (if (> m upper)
      (multiply (normalize v)  upper)
      v)))

(defn with-magnitude [v mag]
  "returns a vector with a certain magnitude"
  (multiply (normalize v) mag ))

; A dot B is equal to the magnitude of A times magnitude of B times 
; cosine of theta (with theta defined as the angle between the 
; two vectors A and B) 
(defn dot-product [v1 v2]
  "dot-product, returns a skalar"
  (+ (* (first v1) (first v2)) (* (second v1) (second v2)))) 

(defn angle-between [v1 v2]
  "returns the angle between two vectors in radians"
  (let [dot-product (dot-product v1 v2)
        cos-theta (/ dot-product (* (magnitude v1) (magnitude v2)))]
    (Math/acos cos-theta)))

(defn heading-2d [v]
  "returns the heading of a vector, 2D radians"
  (let [angle (Math/atan2 (* (second v) -1.0) (first v))]
    (* angle -1.0)))

(defn random-2d []
  "returns a vector with random heading, 2D radians"
  (normalize [(- (rand 2) 1.0) (- (rand 2) 1.0)]))