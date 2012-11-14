(ns nature-of-code.forces.fluidresistance.core
  "Demonstration of multiple force acting on bodies (Mover class)
   Bodies experience gravity continuously
   Bodies experience fluid resistance when in water"
  (:require [quil.core :as q]
            [nature-of-code.forces.fluidresistance.scene :as scene]
            [nature-of-code.forces.fluidresistance.mover :as mover])
  (:import [processing.core PVector]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(def params 
  {:size [800 200]
   :background 255
   :frame-rate 30
   :mover-count 5
   :mover-r 48
   :speed-x 0
   :speed-y 0
   :topspeed 5 
   :acceleration 0.2})

(def movers
  (atom
    (map (fn [id]
           {:id id
            :location (PVector. (rand-int (first (params :size))) (rand-int (second (params :size)))) 
            :velocity (PVector. (params :speed-x) (params :speed-y))})
         (range (params :mover-count)))))

(def scene-graph
  (let [mover-node (mover/make-mover-node)]
        (scene/->GraphNode
          []
          identity
          mover/draw)]
    mover-node))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn calc-acceleration 
  "Compute acceleration-vector that points from source to target
  set magnitude of acceleration in params"
  [source-location target-location]
  (let [ target-v (PVector/sub target-location source-location)
         target-nv (do (.normalize target-v) target-v) ; Seiteneffekt !
         ; Set magnitude of acceleration
         acceleration (PVector/mult target-nv (float (params :acceleration)))]
    acceleration))

(defn calc-velocity
  "Compute new velocity-vector by adding acceleration
   and constrain with topspeed"
  [velocity acceleration]
  (let [a-velocity (PVector/add velocity acceleration)
        l-velocity (do (.limit a-velocity (float (params :topspeed))) a-velocity)]
    l-velocity)) 
        
(defn update-mover [mover target-location]
  (let [ 
        m-id (:id mover)
        m-location (:location mover)
        m-velocity (:velocity mover)
        ; Location changes by velocity
        next-location (PVector/add m-location m-velocity)
        ; Velocity changes by acceleration
        acceleration (calc-acceleration m-location target-location)
        next-velocity (calc-velocity m-velocity acceleration)]
    { :id m-id :location next-location :velocity next-velocity }))

(defn update-movers [movers target-location] 
  (map #(update-mover % target-location) movers))

(defn draw []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  ; draw movers
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill (q/random 100 200))
  
  (dorun (map #(scene/draw scene-graph (:location %)) @movers))

  ; draw hint(s)
  (q/fill 0)
  (q/text "click mouse to reset" 10 30)

  ; update movers to next state
  (swap! movers #(update-movers % (PVector. (q/mouse-x) (q/mouse-y))))) 

(q/defsketch fluidresistance
  :title "Bodies experience gravity and fluid resistance"
  :setup setup
  :draw draw
  :size (params :size))
