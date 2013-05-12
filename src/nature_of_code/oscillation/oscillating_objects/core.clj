(ns nature-of-code.oscillation.oscillating-objects.core
  "Demonstration of oscillation. Objects in randomly oscillating motion 
  are connected by strings to a center-point. Looks like a spider moving its legs. 
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as qc]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size-x 600 
   :size-y 200
   :background 255
   :frame-rate 30
   :oscillator-count 8
   :oscillator-r 32
   :oscillator-color 127
   :oscillator-opacity 127
   :max-speed-x 0.05
   :max-speed-y 0.05
   :min-amplitude-x 20
   :min-amplitude-y 20})

;;
;; Abstractions
;;

(defprotocol Mobile
  (move [this] "calc next motion state for the mobile object"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Oscillator
;;

(defrecord Oscillator [id angle velocity amplitude lifespan]
  Mobile
  (move [oscillator]
    (let [next-angle (mv/add (:angle oscillator) (:velocity oscillator))]
      (assoc oscillator :angle next-angle)))
  
  Drawable
  (draw [oscillator]
    (qc/push-matrix)
    (qc/translate (/ (qc/width) 2) (/ (qc/height) 2))
    (qc/stroke 0)
    (qc/stroke-weight 2)
    (qc/fill (params :oscillator-color) (params :oscillator-opacity))
    (let [amplitude-x (first (:amplitude oscillator))
          amplitude-y (second (:amplitude oscillator))
          angle-x (first (:angle oscillator))         
          angle-y (second (:angle oscillator))        
          x (* (Math/sin angle-x) amplitude-x)
          y (* (Math/cos angle-y) amplitude-y)] 
      (qc/line 0 0 x y)
      (qc/ellipse x y  (params :oscillator-r) (params :oscillator-r)))
    (qc/pop-matrix)))

(defn make-oscillators []
  (let [max-speed-x (params :max-speed-x)
        max-speed-y (params :max-speed-y)
        min-amplitude-x (params :min-amplitude-x)
        min-amplitude-y (params :min-amplitude-y)]
    (map 
      (fn [id]
        (map->Oscillator 
          {:id (str "osc" id)
           :angle [0 0]
           :velocity [(qc/random (* max-speed-x -1) max-speed-x) (qc/random (* max-speed-y -1) max-speed-y)]
           :amplitude [(qc/random min-amplitude-x (/ (qc/width) 2)) (qc/random min-amplitude-y (/ (qc/height) 2))]}))
      (range (params :oscillator-count)))))

;;
;; Sketch
;;

(def sketch-model
  (atom
    {:oscillators nil}))

(defn init-model [m-atom] 
  (swap! 
    m-atom assoc :oscillators (make-oscillators)) 
  m-atom)

(defn update-model [m-atom]
  (swap!
    m-atom
    #(assoc % :oscillators (map move (:oscillators %))))
  m-atom)

(defn setup-sketch []
  (qc/frame-rate (params :frame-rate))
  (qc/smooth) ; anti aliasing on
  (init-model sketch-model))

(defn draw-sketch []
  (qc/background (params :background))
  (update-model sketch-model)
  (dorun (map draw (:oscillators @sketch-model))))

(defn run-sketch []
  (qc/defsketch oscillating-objects
    :title "Randomly oscillating objects connected by Strings"
    :setup setup-sketch
    :draw draw-sketch
    :size [(params :size-x) (params :size-y)]))