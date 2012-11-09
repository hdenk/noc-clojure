(ns nature-of-code.fluidresistance
  "Demonstration of multiple force acting on bodies (Mover class)
   Bodies experience gravity continuously
   Bodies experience fluid resistance when in water"
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [800 200]
   :background 255
   :frame-rate 30
   :mover-count 5
   :mover-rx 48
   :mover-ry 48
   :speed-x 0
   :speed-y 0
   :topspeed 5
   :acceleration 0.2})

(def ^:dynamic *reset-movers* false)

(defn make-mover []
  (let [
        location (PVector. (int (rand (first (params :size)))) (int (rand (second (params :size))))) 
        velocity (PVector. (params :speed-x) (params :speed-y))]
    (atom { :location location :velocity velocity })))

(defn reset-mover [mover]
  (reset! mover (deref (make-mover))))

(defn reset-movers [mover-seq]
  (dorun (map #(reset-mover %) mover-seq))) ; dorun returns nil

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn reset []
  (alter-var-root (var *reset-movers*) (fn [_] true)))

(defn mouse-pressed []
  (reset))

(defn update-mover-velocity [mover acceleration]
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

(defn update-mover-location [mover]
  (swap! 
    mover 
    update-in 
    [:location] 
    #(PVector/add %1 %2) 
    (:velocity @mover))
  mover)

(defn update-mover [mover x y]
  ; Velocity changes according to acceleration
  (let [
        ; Compute a vector that points from mover to target
        target-v (PVector/sub (PVector. x y) (:location @mover))
        target-nv (do (.normalize target-v) target-v) ; Seiteneffekt !
        ; Set magnitude of acceleration
        acceleration (PVector/mult target-nv (float (params :acceleration)))]
    (update-mover-velocity mover acceleration))

  ; Location changes by velocity
  (update-mover-location mover)
  mover) 

; update and render single mover this gives movers
; different alpha transparency
(defn render [mover]
  (q/no-stroke)
  (q/fill 255 100) ; fill with alpha transparency
  (q/rect 0 0 (q/width) (q/height))

  (update-mover mover (q/mouse-x) (q/mouse-y))

  ; Display mover at its location
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)
  (q/ellipse (.-x (:location @mover)) (.-y (:location @mover)) (params :mover-rx) (params :mover-ry))

  (q/fill 0)
  (q/text "click mouse to reset" 10 30))


(defn gen-draw-fn [] 
  "gen function that renders the output"
  (let [mover-seq (repeatedly (params :mover-count) make-mover)]
    (fn draw[] 
      (when *reset-movers*
          (reset-movers mover-seq)
          (alter-var-root (var *reset-movers*) (fn [_] false)))
      (dorun (map render mover-seq))))) ; dorun returns nil

(q/defsketch fluidresistance
  :title "Bodies experience gravity and fluid resistance"
  :setup setup
  :draw (gen-draw-fn)
  :size (params :size)
  :mouse-pressed mouse-pressed)
