(ns nature-of-code.introduction.random-distribution.core
  "visualize distribution of random numbers"
  (:require [quil.core :as q]))

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
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)

  (let [rc-count (count @random-counts)]
    ; Pick a random number and increase the count
    (let [index (int (q/random rc-count))]
      (swap! random-counts update-in [index] inc))

    ; Draw a rectangle to graph results
    (let [w (/ (q/width) rc-count)]
      (dotimes [x rc-count] 
        (let [r-count (get @random-counts x)]
          (q/rect (* x w) (- (q/height) r-count) (dec w) r-count)))))
  random-counts) 

(defn gen-draw-fn [] 
  "gen function that renders the output"
  (let [random-counts (make-random-counts)] ; create state
    (fn [] (render random-counts)))) ; and work with it

(q/defsketch random-walk
  :title "random-distribution"
  :setup setup
  :draw (gen-draw-fn)
  :size (params :size))
