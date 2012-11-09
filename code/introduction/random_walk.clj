(ns nature-of-code.random-walk
  "Random Walker (No Vectors)"
  (:require [quil.core :as q]))

(def params 
  {:size [400 400]
   :background 0
   :frame-rate 30
   :rect-width 20
   :rect-height 20})

(defn make-walker []
  (atom 
    {:x (/ (first (params :size)) 2)
     :y (/ (second (params :size)) 2)}))

(defn walk [walker]
  "Randomly move up, down, left, right, or stay in one place"
  (let [dx (q/random -2 2)
        dy (q/random -2 2)
        x (+ (:x @walker) dx)
        y (+ (:y @walker) dy)]
    (swap! walker assoc 
           :x (q/constrain x 0 (dec (q/width)))
           :y (q/constrain y 0 (dec (q/height)))))
  walker) 

(defn setup []
  (q/frame-rate (params :frame-rate)))

(defn render [walker]
  (q/background (params :background)) 
  (q/stroke 0)
  (q/fill 175)
  (q/rect-mode processing.core.PConstants/CENTER)
  (q/rect (:x @walker), (:y @walker), (params :rect-width), (params :rect-height)))

(defn gen-draw-fn [] 
  "Run the walker object"
  (let [walker (make-walker)] ; create state
    (fn [] (-> walker (walk) (render))))) ; and work with it 

(q/defsketch random-walk
  :title "random-walk"
  :setup setup
  :draw (gen-draw-fn)
  :size (params :size))
