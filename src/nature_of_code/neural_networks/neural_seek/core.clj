(ns nature-of-code.neural-networks.neural-seek.core
  "Neural Agent steers towards multiple Targets, uses simple Perceptron plus Reinforcement
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size-x 600 
   :size-y 400
   :background 255
   :frame-rate 30
   :learning-rate 0.001
   :max-speed 4
   :max-force 0.1
   :target-count 5
   :target-r 30
   :vehicle-r 6
   :rect-s 36
   :vehicle-color 127}) 

;;
;; Protocols
;;

(defprotocol NeuralNetwork
  (train [this forces desired] "train the neural network"))

(defprotocol FeedForward
  (feed-forward [this forces]))

(defprotocol Mobile
  (move [this] "calc next motion state for the mobile object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Autonomous
  (steer [this targets desired-location] "calculates and applies the steering-force, calls training of perceptron")
  (seek [this target] "calculates a steering force towards a target"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Perceptron
;;

(defn random [min max]
  (+ min (rand (- max min))))

(defn next-weight [force weight error learning-rate]
  (let [weight-plus-dx (+ weight (* learning-rate (first error) (first force)))
        next-weight (+ weight-plus-dx (* learning-rate (second error) (second force)))]
    (q/constrain next-weight 0 1)))

(defrecord Perceptron [weights learning-rate]
  NeuralNetwork
  (train [this forces error]
    (let [next-weights (into []
                             map
                             #(next-weight %1 %2 error (:learning-rate this))
                             forces       
                             (:weights this))]
      (assoc this :weights next-weights)))

  FeedForward
  (feed-forward [this forces]
    (let [sum (reduce 
                mv/add 
                (map mv/multiply forces (:weights this)))]
      sum))) ; no activation-function here

(defn random-weights [weights-count]
  (into [] (take weights-count (repeatedly #(random 0 1)))))

(defn gen-perceptron 
  [& {:keys [weights learning-rate] 
      :or {weights [] learning-rate 0.0}}] 
  (Perceptron. weights learning-rate))

;;
;; Vehicle
;;

(defn train [vehicle forces error]
  (let [next-perceptron (train (:perceptron vehicle) forces error)]
    (assoc vehicle :perceptron next-perceptron)))

(defrecord Vehicle [id mass location velocity acceleration r max-speed max-force perceptron]
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
  (steer [this targets desired-location]
    (let [forces (map #(seek this %) targets)
          steering-force (feed-forward (:perceptron this) forces)
          error (mv/subtract desired-location (:location this))]
      (-> this 
          (apply-force steering-force)
          (train forces error))))

  (seek [this target]
    ; Normalize desired and scale to maximum speed
    (let [distance (mv/subtract target location)
          distance-n (mv/normalize distance)
          desired (mv/multiply distance-n (float max-speed))
          steer (mv/subtract desired velocity) ; Steering = Desired minus velocity
          limited-steer (mv/limit steer max-force)] ; Limit to maximum steering force
      limited-steer))

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

(defn gen-vehicle 
  [& {:keys [id mass location velocity acceleration r max-speed max-force perceptron] 
      :or {id "vx" mass 0.0 location [0 0] velocity [0.0 0.0] acceleration [0.0 0.0] 
           r 0 max-speed 0.0 perceptron nil}}] 
  (Vehicle. id mass location velocity acceleration r max-speed max-force perceptron))

(defn do-gen-vehicle [] 
  (gen-vehicle :id "v1" :mass 1.0 :location [(/ (params :size-x) 2) (/ (params :size-y) 2)]
               :velocity [0 -2] :acceleration [0 0] 
               :r (params :vehicle-r) :max-speed (params :max-speed) :max-force (params :max-force)
               :perceptron (gen-perceptron (random-weights) (params :learning-rate))))  

;;
;; Target
;;

(defrecord Target [id location])

(defn gen-target 
  [& {:keys [id location] 
      :or {id "tx" location [0 0]}}] 
  (Target. id location))

(defn do-gen-targets [target-count]
  (into [] 
        (map
          #(gen-target :id (str "v" %) :location [(rand-int (q/width)) (rand-int (q/height))])
          (range target-count))))

;;
;; Sketch
;;

(def sketch-model 
  (atom
    {:targets nil
     :vehicle nil
     :desired-location nil}))

(defn init-sketch-model [m-atom]
  (swap! m-atom #(assoc % :targets (do-gen-targets (params :target-count))))
  (swap! m-atom #(assoc % :vehicle (do-gen-vehicle)))
  (swap! m-atom #(assoc % :desired-location [(/ (q/width) 2) (/ (q/height) 2)])))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)
  (init-sketch-model sketch-model))

(defn draw-sketch []
  (q/background (params :background))

  ; Draw a rectangle to show the Vehicle's goal
  (q/rect-mode :center)
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 0, 100)
  (let [desired-x (first (:desired-location @sketch-model))
        desired-y (second (:desired-location @sketch-model))]
    (q/rect desired-x  desired-y, (params :rect-s), (params :rect-s)))

  ;Draw the targets
  (loop [target-index 0]
    (q/fill 0, 100)
    (q/stroke 0)
    (q/stroke-weight 2)
    (let [target (nth (:targets @sketch-model) target-index)
          target-x (first target)
          target-y (second target)]
      (q/ellipse target-x, target-y, (params :target-r), (params :target-r)))
    (when (< target-index (dec (params :target-count)))
      (recur (inc target-index))))

  (let [vehicle (:vehicle @sketch-model)
        targets (:targets @sketch-model)
        desired-location (:desired-location @sketch-model)]
    ; calc steering and move vehicle
    (swap! 
      sketch-model 
      #(assoc 
         % 
         :vehicle 
         (-> (:vehicle @%) 
             (steer targets desired-location)
             (move))))

    ; Draw the Vehicle
    (draw vehicle)))

(defn mouse-pressed []
  (swap! 
    sketch-model 
    #(assoc 
       % 
       :targets 
       (do-gen-targets (params :target-count)))))

(defn run-sketch []
  (q/defsketch neural-seek 
    :title "Neural Agent steers towards multiple Targets, uses simple Perceptron plus Reinforcement"
    :setup setup-sketch
    :draw draw-sketch
    :mouse-pressed mouse-pressed
    :size [(params :size-x) (params :size-y)]))
