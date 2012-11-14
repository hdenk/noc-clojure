(ns nature-of-code.forces.fluidresistance.mover
  (:require [quil.core :as q]
            [nature-of-code.forces.fluidresistance.view-graph :as view-graph])
  (:import [processing.core PVector]))

(defrecord Mover [id location velocity color])

(defn make-mover 
  [id location velocity color] 
  (Mover. id location velocity color))

(defn update [{:keys [id location velocity color]}]
  (let [; Location changes by velocity
        next-location (PVector/add m-location m-velocity)
        ; Velocity changes by acceleration
        acceleration (calc-acceleration m-location target-location)
        next-velocity (calc-velocity m-velocity acceleration)]
    (make-mover id next-location next-velocity color)))

(defn draw [{:keys [location color]}]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill color)
  (q/ellipse (.-x location) (.-y location) (core/params :mover-r) (core/params :mover-r)))

(defn make-graph-node
  (view-graph/make-graph-node [] identity draw))
