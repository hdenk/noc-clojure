(ns nature-of-code.agents.separate-and-seek.core
  "Primitive Agent steers towards Target 
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(def params 
  {:size-x 600 
   :size-y 400
   :background 255
   :frame-rate 30
   :max-speed 4
   :max-force 0.3
   :vehicle-count 10
   :vehicle-r 6
   :vehicle-color 127
   :seek-factor 1
   :separation-factor 2}) 

;;
;; Protocols
;;

(defprotocol Mobile
  (move [this] "calc next motion state for the mobile object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Autonomous
  (seek [this target] "steer towards a target object by applying force")
  (separate [this targets] "keep a min-distance to other vehicles"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Vehicle
;;

(defn sum-distances [sum [location1 location2]]
  (let [separation-r (* (params :vehicle-r) 2)
        d (mv/distance location1 location2)]
    (if (and (> d 0) (< d separation-r))
      (let [diff (mv/subtract location1 location2)
            diff-n (mv/normalize diff)
            weighted-distance (mv/divide diff-n d)]
        (mv/add sum weighted-distance)))))
  
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
  (seek [this target]
    ; Normalize desired and scale to maximum speed
    (let [distance (mv/subtract target location)
          distance-n (mv/normalize distance)
          desired (mv/multiply distance-n (float max-speed))
          steer (mv/subtract desired velocity) ; Steering = Desired minus velocity
          limited-steer (mv/limit steer max-force)] ; Limit to maximum steering force
      limited-steer))
  
  (separate [this other-vehicles]
    (let [separation-r (* (params :vehicle-r) 2)
          distance-sum (reduce sum-distances [0 0] (map #(vector (:location %1) %2) other-vehicles (repeat location)))
          distance-sum-n (mv/normalize distance-sum)
          distance-sum-max (mv/multiply distance-sum max-speed)
          steering-force (mv/subtract distance-sum-max velocity)
          steering-force-l (mv/limit steering-force max-force)]
      steering-force-l))
             
  Drawable
  (draw [this]
    (q/stroke 0)
    (q/stroke-weight 1)
    (q/fill (params :vehicle-color))

    (q/push-matrix)  
    (q/translate (first location) (second location))
    (q/ellipse 0 0 (params :vehicle-r) (params :vehicle-r))
    (q/pop-matrix)
    this))

(defn gen-vehicle 
  [& {:keys [id mass location velocity acceleration r max-speed max-force] 
      :or {id "vx" mass 0.0 location [0 0] velocity [0.0 0.0] acceleration [0.0 0.0] 
           r 0 max-speed 0.0}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force))

(defn gen-and-init-vehicles [vehicle-count [size-x size-y]] 
    (into [] 
        (map
          #(gen-vehicle :id (str "v" %) :mass 1.0 :location [(rand-int size-x) (rand-int size-y)]
                        :r (params :vehicle-r) :max-force (params :max-force) :max-speed (params :max-speed))
          (range vehicle-count))))

;;
;; Sketch
;;

(def sketch-model
  (atom
    {:vehicles nil}))

(defn init-sketch-model [m-atom sketch-size]
  (swap! m-atom #(assoc % :vehicles (gen-and-init-vehicles (params :vehicle-count) sketch-size))))
    
(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)
  (init-sketch-model sketch-model [(q/width) (q/height)]))

(defn next-state [vehicle other-vehicles desired-location]
  (let [seek-force (seek vehicle desired-location)
        separate-force (separate vehicle other-vehicles)
        seek-force-f (mv/multiply seek-force (params :seek-factor))
        separate-force-f (mv/multiply separate-force (params :separation-factor))]
    (-> vehicle
      (apply-force seek-force-f)
      (apply-force separate-force-f)
      (move))))

(defn draw-sketch []
  ; draw background
  (q/background (params :background))

  ; calc, draw and forward to next state  
  (let [vehicles (:vehicles @sketch-model)
        next-vehicles (into [] (map
                                 #(next-state % vehicles [(q/mouse-x) (q/mouse-y)])
                                 vehicles))]
    ; draw vehicles
    (dorun (map draw vehicles))

    ; forward to next state   
    (swap! 
      sketch-model 
      #(assoc 
         % 
         :vehicles next-vehicles)))

    ; Display some info
    (q/fill 127)
    (q/text "Drag the mouse to generate new vehicles" 10 18))

(defn mouse-dragged []
  (swap! 
    sketch-model 
    #(assoc 
       % 
       :vehicles (gen-and-init-vehicles (params :vehicle-count) [(q/width) (q/height)]))))

(defn run-sketch []
  (q/defsketch separate-and-seek 
    :title "Multiple agents are attracted by mouse and try to keep separate"
    :setup setup-sketch
    :draw draw-sketch
    :mouse-dragged mouse-dragged
    :size [(params :size-x) (params :size-y)]))