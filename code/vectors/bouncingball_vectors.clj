(ns nature-of-code.bouncingball-vectors
  "Example 1-2: Bouncing Ball, with PVector!"
  (:require [quil.core :as q]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(def params 
  {:size [200 200]
   :background 255
   :frame-rate 30
   :ball-x 100
   :ball-y 100
   :speed-x 2.5
   :speed-y 5})

(defn make-ball []
  (let [location (processing.core.PVector. (params :ball-x) (params :ball-y))
        velocity (processing.core.PVector. (params :speed-x) (params :speed-y))]
    (atom { :location location :velocity velocity })))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/background (params :background))
  (q/smooth))

(defn render [ball]
  (q/no-stroke)
  (q/fill 255 10)
  (q/rect 0 0 (q/width) (q/height))

  (let [location (:location @ball)]
    (if (or 
          (> (.-x location) (q/width)) 
          (< (.-x location) 0))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        (fn [v-old] (processing.core.PVector. (* (.-x v-old) -1) (.-y v-old)))))

    (if (or 
          (> (.-y location) (q/height)) 
          (< (.-y location) 0))
      (swap! 
        ball 
        update-in 
        [:velocity] 
        (fn [v-old] (processing.core.PVector. (.-x v-old) (* (.-y v-old) -1))))))

  (let [velocity (:velocity @ball)]
    (swap! 
      ball 
      update-in 
      [:location] 
      #(processing.core.PVector/add %1 %2) velocity))

  ; Display circle at x location
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (.-x (:location @ball)) (.-y (:location @ball)) 16 16))

(defn gen-draw-fn [] 
  "gen function that renders the output"
  (let [ball (make-ball)] ; create state
    (fn [] (render ball)))) ; and work with it

(q/defsketch random-walk
  :title "Bouncing Ball with Vectors"
  :setup setup
  :draw (gen-draw-fn)
  :size (params :size))

