(ns nature-of-code.bouncingball-vectors
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
   :speed-y 5})

(defn make-ball []
  (let [location (PVector. (params :ball-x) (params :ball-y))
        velocity (PVector. (params :speed-x) (params :speed-y))]
    (atom { :location location :velocity velocity })))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn- check-edges [ball]
  (let [location (:location @ball)]
    (if (or 
          (> (.-x location) (q/width)) 
          (< (.-x location) 0))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(PVector. (* (.-x %) -1) (.-y %))))

    (if (or 
          (> (.-y location) (q/height)) 
          (< (.-y location) 0))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        #(PVector. (.-x %) (* (.-y %) -1))))))

(defn- move [ball]
  (let [velocity (:velocity @ball)]
    (swap! 
      ball 
      update-in 
      [:location] 
      #(PVector/add %1 %2) velocity)))

(defn render [ball]
  (q/no-stroke)
  (q/fill 255 10)
  (q/rect 0 0 (q/width) (q/height))

  (check-edges ball)
  (move ball)

  ; Display circle at ball location
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (.-x (:location @ball)) (.-y (:location @ball)) (params :ball-r) (params :ball-r)))

(defn gen-draw-fn [] 
  "gen function that renders the output"
  (let [ball (make-ball)] ; create state
    (fn [] (render ball)))) ; and work with it

(q/defsketch bouncing-ball
  :title "Bouncing Ball with Vectors"
  :setup setup
  :draw (gen-draw-fn)
  :size (params :size))

