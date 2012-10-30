(ns nature-of-code.random-walk
  "Random Walker (No Vectors)"
  (:use quil.core))

; TODO entfernen ?
(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 
 
(def params 
  {:size [400 400]
   :background 0
   :frame-rate 30})

(defn make-walker []
  (atom 
    {:x (/ (first (params :size)) 2)
     :y (/ (second (params :size)) 2)}))

(defn walk [walker]
  "Randomly move up, down, left, right, or stay in one place"
  (let [dx (- 2 (rand 4))
        dy (- 2 (rand 4))
        x (+ (:x @walker) dx)
        y (+ (:y @walker) dy)]
    (swap! walker assoc :x (constrain x 0 (dec (width))))
    (swap! walker assoc :y (constrain y 0 (dec (height)))))) 

(defn render [walker]
  (stroke 0)
  (fill 175)
  (rect-mode processing.core.PConstants/CENTER)
  (rect (:x walker), (:y walker), 40, 40))

(defn gen-draw-fn [walker] 
  "Run the walker object"
    (fn []
      (do (dbg walker) (-> walker (walk) (render))))) ; TODO remove dbg

(defn setup []
  (frame-rate (params :frame-rate))
  (background (params :background)))

(defsketch random-walk
  :title "random-walk"
  :setup setup
  :draw (gen-draw-fn (make-walker))
  :size(params :size) )
