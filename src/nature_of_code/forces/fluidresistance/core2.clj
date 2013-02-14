(ns nature-of-code.forces.fluidresistance.core2
  "Demonstration of multiple force acting on bodies (Mover class)
   Bodies experience gravity continuously
   Bodies experience fluid resistance when in water
   Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as qc])
  (:use [nature-of-code.math.vector :as mv]))

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
   :mover-count 5 
   :mass-classes 3 
   :initial-speed-x 0
   :initial-speed-y 0
   :initial-acceleration-x 0
   :initial-acceleration-y 0 
   :drag-coefficient 0.2
   :mover-color 127
   :fluid-color 211}) 

;;;
;;; Mover
;;;

(defrecord Mover [id mass location velocity acceleration color])

(defn apply-force [{:keys [acceleration mass] :as mover} force] 
  "takes a mover and a force, applies the force and returns a mover with changed acceleration"
  ; Newton's 2nd law: F = M * A
  ; or A = F / M"
	(let [f (mv/divide force (float mass))
	      next-acceleration (mv/add acceleration f)]
	  (assoc mover :acceleration next-acceleration)))
 
(defn apply-gravity [mover]
  ; Gravity is scaled by mass here!
  (let [gravity (PVector. 0 (* 0.1 (:mass mover)))]
    (apply-force mover gravity)))

(defn update-motion-state 
  "takes a mover and force returns a mover with updated motion-state"
   [{:keys [location velocity acceleration] :as mover}]
    (let [next-location (mv/add location velocity)
          next-velocity (mv/add velocity acceleration)
          next-acceleration (mv/multiply acceleration (float 0))]
      (assoc mover :location next-location :velocity next-velocity :acceleration next-acceleration)))


(defn apply-drag-force [mover]
  (let [fluid (@view-model :fluid)]
    ; Is the Mover in the liquid ?
    (if (contains-mover? fluid mover)
      (let [drag-force (drag-force fluid mover)]
        (apply-force mover drag-force))
      mover)))


 (defn update-mover
   "takes a mover and force returns a mover with updated motion-state and applied force"
   [mover fluid]
      (-> (update-motion-state mover)
          (apply-force force)))
  
(defn draw-mover 
  [{:keys [location mass color] :as mover}]
(dbg mover)
  (qc/stroke 0)
  (qc/stroke-weight 2)
  (qc/fill color, 200)
  (let [[x y] location]
    (qc/ellipse x y (* mass 16) (* mass 16)))
  mover)

(defn make-movers []
  (map 
    (fn [id]
      (map->Mover 
        {:id (str "mover" id)
         :mass (inc (rand-int (params :mass-classes)))              
         :location [(rand-int (params :size-x)) (/ (rand-int (params :size-y)) 2)] 
         :velocity [(params :initial-speed-x) (params :initial-speed-y)]
         :acceleration [(params :initial-acceleration-x) (params :initial-acceleration-y)]
         :color (params :mover-color)}))
    (range (params :mover-count))))

;;;
;;; Fluid
;;;

(defrecord Fluid [id x y width height color drag-coefficient])
  
(defn fluid-contains-mover? 
  "takes a fluid and a mover and returns true, if the mover is inside the fluid" 
  [{:keys [x y width height]} {mover-location :location}]
  (let [[mover-x mover-y] mover-location]
    (if (and 
          (> mover-x x) (< mover-x (+ x width)) 
          (> mover-y y) (< mover-y (+ y height))) 
      true
      false)))                                                                  

(defn fluid-drag-force 
  "takes a fluid and a mover and returns drag-force" 
  [{c :drag-coefficient} {velocity :velocity}] 
  (let [speed (mv/magnitude velocity)
        drag-magnitude (* c speed speed)
        drag-force (mv/multiply velocity (float -1))] 
    (-> (mv/normalize drag-force)
        (mv/multiply (float drag-magnitude)))))

(defn draw-fluid
  [{:keys [x y width height color] :as fluid}]
(dbg fluid)
    (qc/no-stroke)
    (qc/fill color)
    (qc/rect x y width height)
    fluid)

(defn make-fluid []
  (map->Fluid 
    {:id "fluid1" 
     :x 0 :y (* (params :size-y) 0.75) :width (params :size-x) :height (params :size-y)  
     :color (params :fluid-color) :drag-coefficient (params :drag-coefficient)}))

;;;
;;; Main
;;;

(def sketch-model
  (atom
    { :fluid nil 
      :movers nil}))

(defn init-sketch-model [m-atom] 
  (swap! 
    m-atom 
    (fn [m] 
      (-> (assoc-in m [:fluid] (make-fluid))
          (assoc-in [:movers] (make-movers))))))

(defn update-movers [movers]
  (map update-mover movers))

(defn setup-sketch []
  (qc/frame-rate (params :frame-rate))
  (dbg (init-sketch-model sketch-model)))

(defn draw-sketch []
  ; draw Background
  (qc/background (params :background))

  ; draw fluid
  (draw-fluid (@sketch-model :fluid))

  ; draw movers
  (dorun (map #(draw-mover %) (@sketch-model :movers)))

  ; draw hint(s)
  (qc/fill 0)
  (qc/text "click mouse to reset" 10 30)

  ; update sketch-model to next state
  (swap! 
    sketch-model 
    #(update-in 
       % 
       [:movers] 
       update-movers)))

(defn mouse-pressed []
  (swap! 
    sketch-model 
    #(update-in 
       % 
       [:movers] 
       (constantly (make-movers)))))

(defn run-sketch []
	(qc/defsketch fluidresistance
	  :title "Bodies experience gravity and fluid resistance"
	  :setup setup-sketch
	  :draw draw-sketch
	  :mouse-pressed mouse-pressed
    :size [(params :size-x) (params :size-y)]))