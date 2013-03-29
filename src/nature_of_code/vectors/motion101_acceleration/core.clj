(ns nature-of-code.vectors.motion101-acceleration.core
  "Demonstration of the basics of motion with vector.
  A 'Mover' object stores location, velocity, and acceleration as vectors
  The motion is controlled by affecting the acceleration (in this case towards the mouse) 
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as qc]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size-x 800 
   :size-y 200
   :background 255
   :frame-rate 30
   :mover-r 48
   :mover-color 127
   :initial-speed-x 0
   :initial-speed-y 0
   :topspeed 5
   :acceleration-rate 0.2})

(defn do-init-mover [width height m]
  (-> (assoc-in m [:location] [(/ width 2.0) (/ height 2.0)])
      (assoc-in [:velocity] [(params :initial-speed-x) (params :initial-speed-y)])))

(defn init-mover [m-atom width height]
  (swap! m-atom (partial do-init-mover width height)))

(defn do-update-mover [m]
  (let [mouse [(qc/mouse-x) (qc/mouse-y)]
        acc (mv/subtract mouse (:location m))
        acc (mv/set-magnitude acc (params :acceleration-rate))]
    (-> (update-in m [:velocity] #(mv/add % acc))
        (update-in [:location] #(mv/add % (:velocity m)))
        (update-in [:velocity] #(mv/limit % (:top-speed m))))))

(defn update-mover [m-atom]
  (swap! m-atom do-update-mover))

(defn draw-mover [m]
  (qc/stroke 0)
  (qc/stroke-weight 2)
  (qc/fill (params :mover-color) 100)
  (apply #(qc/ellipse %1 %2 (params :mover-r) (params :mover-r)) (:location m)))

(def mover (atom {:location []
                  :velocity []
                  :top-speed (params :topspeed)}))

(defn setup-sketch []
  (qc/frame-rate (params :frame-rate))
  (qc/smooth) ; anti aliasing on
  (init-mover mover (params :size-x) (params :size-y)))

(defn draw-sketch []
  (qc/background (params :background))
  (update-mover mover)
  (draw-mover @mover))

(defn run-sketch []
  (qc/defsketch motion-101
    :title "motion-controll by acceleration"
    :setup setup-sketch
    :draw draw-sketch
    :size [(params :size-x) (params :size-y)]))
