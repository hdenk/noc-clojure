;; Based on the Nature of Code
;; by Daniel Shiffman
;; http://natureofcode.com

(ns nature-of-code.math.vector
  "A minimalistic Implementation of basic Vector-Math" 
  (:require [clojure.math.numeric-tower :as math]))

(defn add [& vs]
  (vec (apply map + vs)))

(defn subtract [& vs]
  (vec (apply map - vs)))

(defn magnitude [v]
 (math/sqrt (reduce + (map #(math/expt % 2) v))))

(defn normalize [v]
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

(defn multiply [v scalar ]
  (vec (map * (repeat scalar) v)))

(defn divide [v scalar]
  (vec (map / v (repeat scalar))))

(defn limit [v upper]
  (let [m (magnitude v)]
    (if (> m upper)
      (multiply (normalize v)  upper)
      v)))

(defn set-magnitude [v mag]
  (multiply (normalize v) mag ))

(defn random-2d []
  (normalize [(- (rand 2) 1.0) (- (rand 2) 1.0)]))

(defn heading-2d [v]
  (let [angle (Math/atan2 (* (second v) -1) (first v))]
    (* angle -1)))