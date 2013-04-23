(ns nature-of-code.neural-networks.simple-perceptron.core
  "Neural Networking - A Simple Perceptron learns to classify 2D-Points 
  according to a linear Function
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(def params 
  {:size-x 800 
   :size-y 200
   :background 255
   :frame-rate 30
   :x-min -400
   :x-max 400
   :y-min -100
   :y-max 100
   :point-r 8
   :inputs-per-neuron 3
   :learning-rate 0.00001
   :training-record-count 2000
   :line-color 127})

;;
;; Protocols
;;

(defprotocol NeuralNetwork
  (train [this inputs desired] "train the neural network")
  (activate [this sum]))

(defprotocol FeedForward
  (feed-forward [this inputs]))

;;
;; Perceptron
;;

(defn random [min max]
  (+ min (rand (- max min))))

(defn f [x]
  "The function that describes a line"
  (+ (* 0.4 x) 1))

(defrecord Perceptron [weights learning-rate]
  NeuralNetwork
  (train [perceptron inputs desired]
    ; guess the result (0, -2, or 2) and mult by learning-rate
    (let [guess (feed-forward perceptron inputs)
          error (- desired guess)
          next-weights (into [] 
                             (map 
                               #(+ %1 (* %2 error (:learning-rate perceptron))) 
                               (:weights perceptron)
                               inputs))]
      (assoc perceptron :weights next-weights)))

  (activate [perceptron sum]
    (if (> sum  0) 
      1
      -1))

  FeedForward
  (feed-forward [perceptron inputs]
    (let [sum (reduce 
                + 
                (map * inputs (:weights perceptron)))]
      (activate perceptron sum))))

(defn random-weights [weights-count]
  (into [] (take weights-count (repeatedly #(random -1 1)))))

(defn gen-perceptron 
  [& {:keys [weights learning-rate] 
      :or {weights [] learning-rate 0.0}}] 
  (Perceptron. weights learning-rate))

;;
;; TrainingRecord
;;

(defrecord TrainingRecord [inputs answer])

(defn gen-training-record 
  [answer & inputs]
  (TrainingRecord. inputs answer)) 

(defn gen-training-data [training-record-count]
  (into []
        (take training-record-count
              (repeatedly    
                #(let [
                       x (random (params :x-min) (params :x-max))
                       y (random (params :x-min) (params :x-max))
                       answer (if (< y (f x)) -1 1)]
                   (gen-training-record answer x y 1))))))

;;
;; Sketch
;;

(def sketch-model 
  (atom
    {:training-data nil
     :training-index 0
     :perceptron nil}))

(defn init-sketch-model [m-atom]
  (swap! m-atom #(assoc % :training-data (gen-training-data (params :training-record-count))))
  (let [weights (random-weights (params :inputs-per-neuron))]
    (swap! m-atom #(assoc % :perceptron (gen-perceptron :weights weights :learning-rate (params :learning-rate))))))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)
  (init-sketch-model sketch-model))

(defn draw-sketch []
  ; draw Background
  (q/background (params :background))
  (q/translate (/ (q/width) 2) (/ (q/height) 2))

  ; Draw the line
  (q/stroke-weight 4)
  (q/stroke (params :line-color))
  (let [x1 (params :x-min)
        y1 (f  x1)
        x2 (params :x-max)
        y2 (f x2)]
    (q/line x1 y1 x2 y2))

  ; Draw the line based on the current weights
  ; Formula is weights[0]*x + weights[1]*y + weights[2] = 0
  (q/stroke 0)
  (q/stroke-weight 1)
  (let [perceptron (:perceptron @sketch-model)
        weights (:weights perceptron)
        x1 (params :x-min)
        y1 (- (- (nth weights 2)) (/ (* (nth weights 0) x1) (nth weights 1))) 
        x2 (params :x-max)
        y2 (- (- (nth weights 2)) (/ (* (nth weights 0) x2) (nth weights 1)))]
    (q/line x1 y1 x2 y2))

  (let [perceptron (:perceptron @sketch-model)
        training-data (:training-data @sketch-model)
        training-index (:training-index @sketch-model)]
    ; Train the Perceptron with one "training" point at a time
    (let [inputs (:inputs (nth training-data training-index))
          answer (:answer (nth training-data training-index))
          next-perceptron (train perceptron inputs answer)
          next-training-index (mod (inc training-index) (count training-data))]
      (swap! sketch-model #(assoc % :perceptron next-perceptron :training-index next-training-index)))

    ; Draw all the points based on what the Perceptron would "guess"
    ; Does not use the "known" correct answer
    (loop [index 0]
      (q/stroke 0)
      (q/stroke-weight 1)
      (q/fill 0)

      (let [training-record (nth training-data index)
            inputs (:inputs training-record)
            guess (feed-forward perceptron inputs)]
        (when (> guess 0) 
          (q/no-fill))
        (q/ellipse (nth inputs 0) (nth inputs 1) (params :point-r) (params :point-r)))
      (when (< index training-index)
        (recur (inc index))))

    ; Display some info
    (q/translate (/ (- (q/width)) 2) (/ (+ (q/height)) 2))
    (q/fill 127) 
    (q/text (str "Training Index: " training-index) 10 -36)
    (q/text (str "Weights: " (:weights perceptron)) 10 -18)))

(defn run-sketch []
  (q/defsketch simple-perceptron 
    :title "A Simple Perceptron learns to classify 2D-Points according to a linear Function"
    :setup setup-sketch
    :draw draw-sketch
    :size [(params :size-x) (params :size-y)]))
