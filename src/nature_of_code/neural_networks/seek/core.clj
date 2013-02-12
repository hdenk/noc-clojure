(ns nature-of-code.neural-networks.seek.core
  "Primitive Agent steers towards Target
	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :max-speed 4
   :max-force 0.3
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

(defprotocol Mobile
  (move [this] "calc next motion state for the mobile object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Autonomous
  (seek [this target] "steer towards a target object by applying force"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defrecord Vehicle [id mass location velocity acceleration r max-speed max-force]
  Mobile  
  (move [this]
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

  Autonomous
  (seek [this target]
    (let [desired (PVector/sub target location)]
      ; Normalize desired and scale to maximum speed
      (.normalize desired) ; Seiteneffekt
      (.mult desired (float max-speed)) ; Seiteneffekt
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

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw-sketch []
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
         (seek target) 
         (move)))))

(defn run []
	(q/defsketch particlesystem-forces 
	  :title "Primitive Agent steers towards Target"
	  :target :none
	  :setup setup-sketch
	  :draw draw-sketch
	  :size (params :size)))

