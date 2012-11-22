(ns nature-of-code.agents.arrive.core
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :max-speed 4
   :max-force 0.3
   :arrive-r 100
   :target-r 48
   :vehicle-r 6
   :vehicle-color 127}) 

(defn size-x []
  (first (params :size)))

(defn size-y []
  (second (params :size)))

;;
;; Vehicle
;;

(defprotocol Stateful
  (next-state [this] "calc next state for the stateful object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol TargetAware
  (arrive [this target] "steer towards the target object by applying force"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defrecord Vehicle [id mass location velocity acceleration r max-speed max-force]
  Stateful  
  (next-state [this]
    (let [next-location (PVector/add location velocity)
          next-velocity (PVector/add velocity acceleration)
          next-acceleration (PVector/mult acceleration (float 0))]
      (.limit next-velocity max-speed) ; Seiteneffekt
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration)))

  Massiv
  (apply-force [this force] 
    (let [f (.get force)
          mf (PVector/div f (float mass))
          next-acceleration (PVector/add acceleration mf)]
      (assoc this :acceleration next-acceleration)))

  TargetAware
  (arrive [this target]
    (let [desired (PVector/sub target location)
          mag-desired (.mag desired)]
      ; Normalize desired and scale to maximum speed
      ; or damp speed towards zero if vehicle is within arrive-r
      (.normalize desired) ; Seiteneffekt
      (if (< mag-desired (params :arrive-r))
        (let [m (q/map-range mag-desired 0 (params :arrive-r) 0 max-speed)]
          (.mult desired m)) ; Seiteneffekt
        (.mult desired (float max-speed))) ; Seiteneffekt

      ; Steering = Desired minus velocity
      (let [steer (PVector/sub desired velocity)]
        (.limit steer max-force) ; Limit to maximum steering force
        (apply-force this steer))))

  Drawable
  (draw [this]
    (q/stroke 0)
    (q/stroke-weight 1)
    (q/fill (params :vehicle-color))

    (q/push-matrix)
    (q/translate (.-x location) (.-y location))
    ; Draw a triangle rotated in the direction of velocity
    (let [theta  (+ (.heading2D velocity) (/ Math/PI 2))]
      (q/rotate theta))
    (q/begin-shape)
    (q/vertex 0 (* (* r -1) 2)) 
    (q/vertex (* r -1) (* r 2))
    (q/vertex r (* r 2))        
    (q/end-shape :close) ; processing.core.PConstants/CLOSE
    (q/pop-matrix)
    this))

;;
;; Sketch
;;

(def vehicle 
  (atom 
    (map->Vehicle 
      {:id "v1" :mass 1.0 :location (PVector. (/ (size-x) 2) (/ (size-y) 2))
       :velocity (PVector. 0 -2) :acceleration (PVector. 0 0) 
       :r (params :vehicle-r) :max-speed (params :max-speed) :max-force (params :max-force)})))  

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  (let [target (PVector. (q/mouse-x) (q/mouse-y))] 
    (q/fill 200)
    (q/stroke 0)
    (q/stroke-weight 2)

    (q/ellipse (.-x target) (.-y target) (params :target-r) (params :target-r))

    (draw @vehicle)

    ; Call the appropriate steering behaviors for our agents
    (swap! 
      vehicle 
      #(-> % 
         (arrive target) 
         (next-state)))))

(q/defsketch particlesystem-forces 
  :title "Primitive Agent steers towards Target"
  :setup setup
  :draw draw
  :size (params :size))

