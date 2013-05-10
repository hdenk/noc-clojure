(ns nature-of-code.cellular-automata.game-of-life.core
  "Implementation of conways game of life  
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]))

(def params ^{:doc "DataStructure representing Params to customize the app"} 
  {:size-x 400 
   :size-y 400
   :background 255
   :frame-rate 30
   :cell-w 16})

;;
;; Game
;;

(defrecord Board [rows columns w cells])

(defn gen-board
  [& {:keys [rows columns w cells] 
      :or {rows 0 columns 0 w 0 cells #{}}}] 
  (Board. rows columns w cells)) 

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

;;
;; Sketch
;;

(def board (atom nil))

(defn random-initial-cells [rows columns]
  (condp = (rand-int 4)
    0 #{[1 0] [1 1] [1 2]} ; alternating-cross     
    1 #{[2 0] [2 1] [2 2] [1 2] [0 1]} ; glider
    2 #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]} ; light-spaceship
    3 (set (for [x (range rows), y (range columns)] [x y])))) ; all cells alive

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)

  (let [rows (/ (params :size-y) (params :cell-w))
        columns (/ (params :size-x) (params :cell-w))]
    (swap! board (constantly (gen-board :rows rows :columns columns :w (params :cell-w) :cells (random-initial-cells rows columns))))))

(defn draw-sketch []
  (q/background 255)
  ; draw cells    
  (let [w (:w @board)]
    (doseq [x (range (:rows @board)) 
            y (range (:columns @board))]
      (if (contains? (:cells @board) [x y]) 
        (q/fill 0) 
        (q/fill 255))
      (q/stroke 0)
      (q/rect (* x w) (* y w) w w))) 

  ; next cells-state    
  (swap! board assoc :cells (step (:cells @board))))

(defn mouse-pressed [] 
  (swap! board assoc :cells (random-initial-cells (:rows @board) (:columns @board))))

(defn run-sketch []
	(q/defsketch conways-game-of-life 
	  :title "Implementation of conways game of life"
	  :setup setup-sketch
	  :draw draw-sketch
	  :mouse-pressed mouse-pressed
	  :size [(params :size-x) (params :size-y)]))