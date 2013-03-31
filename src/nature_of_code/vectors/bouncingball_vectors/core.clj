(ns nature-of-code.vectors.bouncingball-vectors.core
  "Example 1-2: Bouncing Ball, with PVector!
	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size [200 200]
   :background 255
   :frame-rate 30
   :ball-x 100
   :ball-y 100
   :ball-r 16
   :speed-x 2.5
   :speed-y 5
   :damping-factor -0.9})

(def ball
  (let [location [(params :ball-x) (params :ball-y)]
        velocity [(params :speed-x) (params :speed-y)]]
    (atom { :location location :velocity velocity })))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn check-edges [ball]
  (let [location (:location @ball)
        velocity (:velocity @ball)]
    (if (or 
          (and (> (first location) (q/width)) (> (first velocity) 0)) 
          (and (< (first location) 0) (< (first velocity) 0)))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(vector (* (first %) (params :damping-factor)) (second %))))

    (if (or 
          (and (> (second location) (q/height)) (> (second velocity) 0)) 
          (and (< (second location) 0) (< (second velocity) 0)))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(vector (first %) (* (second %) (params :damping-factor))))))
  ball)

(defn move [ball]
  (let [velocity (:velocity @ball)]
    (swap! 
      ball 
      update-in 
      [:location] 
      #(mv/add %1 %2) velocity))
  ball)

(defn draw []
  (q/no-stroke)
  (q/fill 255 10)
  (q/rect 0 0 (q/width) (q/height))

  (check-edges ball)
  (move ball)

  ; Display circle at ball location
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (first (:location @ball)) (second (:location @ball)) (params :ball-r) (params :ball-r)))

(defn run-sketch []
	(q/defsketch bouncing-ball
	  :title "Bouncing Ball with Vectors"
	  :setup setup
	  :draw draw
	  :size (params :size)))