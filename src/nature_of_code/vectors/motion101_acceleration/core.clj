(ns nature-of-code.vectors.motion101-acceleration.core
  "Demonstration of the basics of motion with vector.
  A 'Mover' object stores location, velocity, and acceleration as vectors
  The motion is controlled by affecting the acceleration (in this case towards the mouse)"
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [800 200]
   :background 255
   :frame-rate 30
   :mover-x 100
   :mover-y 100
   :mover-rx 48
   :mover-ry 48
   :speed-x 0
   :speed-y 0
   :topspeed 5
   :acceleration 0.2})

(def mover
  (let [location (PVector. (params :mover-x) (params :mover-y))
        velocity (PVector. (params :speed-x) (params :speed-y))]
    (atom { :location location :velocity velocity })))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn update-velocity [mover acceleration]
  (swap! 
    mover 
    update-in 
    [:velocity] 
    #(let [velocity (PVector/add %1 %2)] 
       ; Limit the velocity by topspeed
       (.limit velocity (params :topspeed)) ; Seiteneffekt !
       velocity)
    acceleration)
  mover)

(defn update-location [mover]
  (swap! 
    mover 
    update-in 
    [:location] 
    #(PVector/add %1 %2) 
    (:velocity @mover))
  mover)

(defn move-to [mover x y]
  ; Velocity changes according to acceleration
  (let [; Compute a vector that points from mover to target
        target-v (PVector/sub (PVector. x y) (:location @mover))
        target-nv (do (.normalize target-v) target-v) ; Seiteneffekt !
        ; Set magnitude of acceleration
        acceleration (PVector/mult target-nv (float (params :acceleration)))]
    (update-velocity mover acceleration))

  ; Location changes by velocity
  (update-location mover)) 

(defn draw []
  (q/no-stroke)
  (q/fill 255 100)
  (q/rect 0 0 (q/width) (q/height))

  (move-to mover (q/mouse-x) (q/mouse-y))

  ; Display mover at its location
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)
  (q/ellipse (.-x (:location @mover)) (.-y (:location @mover)) (params :mover-rx) (params :mover-ry)))

(q/defsketch motion101-acceleration
  :title "motion-controll by acceleration"
  :setup setup
  :draw draw
  :size (params :size))
