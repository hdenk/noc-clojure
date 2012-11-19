(ns nature-of-code.forces.fluidresistance.core
  "Demonstration of multiple force acting on bodies (Mover class)
   Bodies experience gravity continuously
   Bodies experience fluid resistance when in water"
   (:require [quil.core :as q])
   (:import [processing.core PVector]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

;;;
;;; Mover
;;;

(defprotocol Stateful
  (next-state [this] "calc next state for the stateful object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the masive object"))

(defprotocol Movable
  (check-edges [this] "check if the drawable object is out ouf bounds and react"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defrecord Mover [id mass location velocity acceleration color]
  Stateful  
  (next-state [this]
    (let [next-location (PVector/add location velocity)
          next-velocity (PVector/add velocity acceleration)
          next-acceleration (PVector/mult acceleration (float 0))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration)))

  Massiv
  (apply-force [this force] 
    ; Newton's 2nd law: F = M * A
    ; or A = F / M"
    (let [f (PVector/div force (float mass))
          next-acceleration (PVector/add acceleration f)]
      (assoc this :acceleration next-acceleration)))

  Movable
  (check-edges [this]
    (if (> (.-y location) (q/height)) 
      (assoc this :location (PVector. (.-x location) (q/height)) :velocity (PVector/mult velocity (float -0.9)))
      this))

  Drawable
  (draw [this]
    (q/stroke 0)
    (q/stroke-weight 2)
    (q/fill color, 200)
    (q/ellipse (.-x location) (.-y location) (* mass 16) (* mass 16))
    this))

;;;
;;; Fluid
;;;

(defprotocol Permeable
  (contains? [this mover] "check if mover is inside of the aetheral object")
  (drag-force [this obj] "calcs the drag-force that affects the mover inside of the aetheral object"))

(defrecord Fluid [id x y width height color drag-coefficient]
  Permeable 
  (contains? 
    [this {mover-location :location}]
    (let [mover-x (.-x mover-location)
          mover-y (.-y mover-location)]
      (if (and 
            (> mover-x x) (< mover-x (+ x width)) 
            (> mover-y y) (< mover-y (+ y height))) 
        true
        false)))                                                                  

  (drag-force [{c :drag-coefficient} {velocity :velocity}] 
    (let [speed (.mag velocity)
          drag-magnitude (* c speed speed)
          drag-force (.get velocity)]
      (.mult drag-force (float -1)) ; Seiteneffekt
      (.normalize drag-force) ; Seiteneffekt
      (.mult drag-force (float drag-magnitude)) ;Seiteneffekt
      drag-force))

  Drawable
  (draw [{:keys [x y width height color]}]
    (q/no-stroke)
    (q/fill color)
    (q/rect x y width height)))

;;;
;;; Main
;;;

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :mover-count 3 
   :initial-speed-x 0
   :initial-speed-y 0
   :initial-acceleration-x 0
   :initial-acceleration-y 0
   :fluid-color 211}) 

(defn make-fluid []
  (map->Fluid 
    {:id "fluid1" 
     :x 0 :y (* (second (params :size)) 0.75) :width (first (params :size)) :height (second (params :size))  
     :color 127 :drag-coefficient 1.0}))

(defn make-movers []
  (map 
    (fn [id]
      (map->Mover 
        {:id (str "mover" id)
         :mass (inc (rand-int 3)) ; TODO            
         :location (PVector. (rand-int (first (params :size))) (/ (rand-int (second (params :size))) 2)) 
         :velocity (PVector. (params :initial-speed-x) (params :initial-speed-y))
         :acceleration (PVector. (params :initial-acceleration-x) (params :initial-acceleration-y))
         :color (params :fluid-color)}))
    (range (params :mover-count))))

(def view-model
  (atom
    { :fluid  (make-fluid)
      :movers (make-movers)}))

(defn apply-gravity [mover]
  ; Gravity is scaled by mass here!
  (let [gravity (PVector. 0 (* 0.1 (:mass mover)))]
    (apply-force mover gravity)))

(defn apply-drag-force [mover]
  (let [fluid (@view-model :fluid)]
    ; Is the Mover in the liquid ?
    (if (contains? fluid mover)
      (let [drag-force (drag-force fluid mover)]
        (apply-force mover drag-force))
      mover)))

(defn movers-next-state [movers]
  (map 
    (comp ; comp wendet fns von rechts nach links an !
      next-state 
      apply-drag-force 
      apply-gravity) 
    movers)) 

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn mouse-pressed []
  (swap! 
    view-model 
    #(update-in 
       % 
       [:movers] 
       (constantly (make-movers)))))

(defn draw []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  ; draw fluid
  (draw (@view-model :fluid))

  ; draw movers
  (dorun (map #(draw %) (@view-model :movers)))

  ; draw hint(s)
  (q/fill 0)
  (q/text "click mouse to reset" 10 30)

  ; update view-model to next state
  (swap! 
    view-model 
    #(update-in 
       % 
       [:movers] 
       movers-next-state)))

(q/defsketch fluidresistance
  :title "Bodies experience gravity and fluid resistance"
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed
  :size (params :size))

