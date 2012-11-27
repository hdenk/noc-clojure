(ns nature-of-code.vectors.bouncingball-vectors.core
  "Example 1-2: Bouncing Ball, with PVector!"
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

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
  (let [location (PVector. (params :ball-x) (params :ball-y))
        velocity (PVector. (params :speed-x) (params :speed-y))]
    (atom { :location location :velocity velocity })))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn check-edges [ball]
  (let [location (:location @ball)
        velocity (:velocity @ball)]
    (if (or 
          (and (> (.-x location) (q/width)) (> (.-x velocity) 0)) 
          (and (< (.-x location) 0) (< (.-x velocity) 0)))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(PVector. (* (.-x %) (params :damping-factor)) (.-y %))))

    (if (or 
          (and (> (.-y location) (q/height)) (> (.-y velocity) 0)) 
          (and (< (.-y location) 0) (< (.-y velocity) 0)))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(PVector. (.-x %) (* (.-y %) (params :damping-factor))))))
  ball)

(defn move [ball]
  (let [velocity (:velocity @ball)]
    (swap! 
      ball 
      update-in 
      [:location] 
      #(PVector/add %1 %2) velocity))
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
  (q/ellipse (.-x (:location @ball)) (.-y (:location @ball)) (params :ball-r) (params :ball-r)))

(q/defsketch bouncing-ball
  :title "Bouncing Ball with Vectors"
  :setup setup
  :draw draw
  :size (params :size))

