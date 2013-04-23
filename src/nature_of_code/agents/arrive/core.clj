(ns nature-of-code.agents.arrive.core
  "Primitive Agent steers towards Target slowing down when comming near
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size-x 600
   :size-y 400
   :background 255
   :frame-rate 30
   :max-speed 4
   :max-force 0.3
   :arrive-r 50
   :target-r 48
   :vehicle-r 6
   :vehicle-color 127}) 

;;
;; Protocols
;;

(defprotocol Mobile
  (move [this] "calc next motion state for the mobile object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Autonomous
  (arrive [this target] "steer towards a target object by applying force"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Vehicle
;;

(defrecord Vehicle [id mass location velocity acceleration r max-speed max-force]
  Mobile 
  (move [this]
    (let [next-location (mv/add location velocity)
          velocity-plus (mv/add velocity acceleration)
          next-velocity (mv/limit velocity-plus max-speed)
          next-acceleration (mv/multiply acceleration (float 0))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration)))

  Massiv
  (apply-force [this force] 
    (let [mf (mv/divide force (float mass))
          next-acceleration (mv/add acceleration mf)]
      (assoc this :acceleration next-acceleration)))

  Autonomous
  (arrive [this target]
    ; Normalize desired and scale to maximum speed
    ; or damp speed towards zero if vehicle is within arrive-r
    (let [distance (mv/subtract target location)
          mag-desired (mv/magnitude distance)
          norm-desired (mv/normalize distance)
          desired (if (< mag-desired (params :arrive-r))
                    (let [m (q/map-range mag-desired 0 (params :arrive-r) 0 max-speed)]
                      (mv/multiply norm-desired m)) 
                    (mv/multiply norm-desired (float max-speed)))]
      (let [steer (mv/subtract desired velocity) ; Steering = Desired minus velocity
            limited-steer (mv/limit steer max-force)] ; Limit to maximum steering force
        (apply-force this limited-steer))))

  Drawable
  (draw [this]
    (q/stroke 0)
    (q/stroke-weight 1)
    (q/fill (params :vehicle-color))

    (q/push-matrix)
    (q/translate (first location) (second location))
    ; Draw a triangle rotated in the direction of velocity
    (let [theta  (+ (mv/heading-2d velocity) (/ Math/PI 2))]
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
      {:id "v1" :mass 1.0 :location [(/ (params :size-x) 2) (/ (params :size-y) 2)]
       :velocity [0 -2] :acceleration [0 0] 
       :r (params :vehicle-r) :max-speed (params :max-speed) :max-force (params :max-force)})))  

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  (let [target [(q/mouse-x) (q/mouse-y)]] 
    (q/fill 200)
    (q/stroke 0)
    (q/stroke-weight 2)

    (q/ellipse (first target) (second target) (params :target-r) (params :target-r))

    (draw @vehicle)

    ; Call the appropriate steering behaviors for our agents
    (swap! 
      vehicle 
      #(-> % 
           (arrive target) 
           (move)))))

(defn run-sketch []
  (q/defsketch particlesystem-forces 
    :title "Primitive Agent steers towards Target"
    :setup setup-sketch
    :draw draw-sketch
    :size [(params :size-x) (params :size-y)]))
