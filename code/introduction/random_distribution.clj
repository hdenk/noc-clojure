(ns nature-of-code.random-distribution
  "visualize distribution of random numbers"
  (:require [quil.core :as q]))

; TODO entfernen ?
(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 
 
(def params 
  {:size [800 200]
   :background 0
   :frame-rate 30})

(defn make-random-counts []
  (atom (vec (repeat 5 (float 0)))))

(defn setup []
  (q/frame-rate (params :frame-rate)))

(defn render [random-counts]
  (q/background (params :background))
  
  ; Draw a rectangle to graph results
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)

  ; Pick a random number and increase the count
  (let [rc-count (count @random-counts)]
    (let [index (int (q/random rc-count))]
      (swap! random-counts update-in [index] inc))

    (let [w (/ (q/width) rc-count)]
      (dotimes [x rc-count]
        (let [r-count (get @random-counts x)]
        (q/rect (* x w) (- (q/height) r-count) (dec w) r-count)))))
  random-counts) ; make cascade-call possible

(defn gen-draw-fn [random-counts] 
  "gen function that renders the output"
    (fn []
      (do (dbg random-counts) (render random-counts)))) ; TODO remove dbg

(q/defsketch random-walk
  :title "random-distribution"
  :setup setup
  :draw (gen-draw-fn (make-random-counts))
  :size (params :size))
