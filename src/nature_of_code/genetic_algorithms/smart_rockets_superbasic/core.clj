(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params ^{:doc "DataStructure representing Params to customize the app"} 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :maxforce 0.1
   :target-r 12
   :rocket-color 127
   :thrusters-color 0}) 

(defn size-x []
  (first (params :size)))

(defn size-y []
  (second (params :size)))

;;
;; Abstractions
;;

(defprotocol Stateful
  (next-state [this] "calc next state for the stateful object"))

(defprotocol Massive
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defprotocol CrossGenetic 
  (crossover [this partner] "produce new DNA by mixing genes of two individuals"))

(defprotocol Mutable
  (mutate [this mutation-rate] "mutate based on probability"))

(defprotocol SelfAdapting
  (check-target [this target] "check if target-location is hit")
  (fitness [this target] "calculate fitness")) 

;;
;; DNA
;;

(defn random-gene [force]
  (let [angle (rand processing.core.PConstants/TWO_PI)
        random-gene (PVector. (Math/cos angle) (Math/sin angle))]
    (.mult random-gene (float (rand force))) ; Seiteneffekt
    random-gene))

(defrecord DNA [maxforce genes]
  CrossGenetic
  (crossover [this partner-dna]
    (let [crossover (rand-int (count (:genes this)))
          child-genes (into 
                        [] 
                        (concat 
                          (first (split-at crossover (:genes this))) 
                          (second (split-at crossover (:genes partner-dna)))))]
      (assoc this :genes child-genes)))

  Mutable
  (mutate [this mutation-rate]
    (let [mutated-genes (reduce 
                          #(if (< (rand) mutation-rate)
                             (conj %1 (random-gene 0.1))
                             (conj %1 %2)) 
                          [] 
                          (:genes this))]
      (assoc this :genes mutated-genes)))) 

(defn random-DNA [lifetime]
  (let [force (rand (params :maxforce))
        genes (repeatedly lifetime #(random-gene force))]
    (DNA. (params :maxforce) genes)))

;;
;; Rocket
;;

(defrecord Rocket [id mass location velocity acceleration r fitness dna gene-counter hit-target]
  Stateful 
  (next-state [this]
    (let [next-location (PVector/add (:location this) (:velocity this))
          next-velocity (PVector/add (:velocity this) (:acceleration this))
          next-acceleration (PVector/mult (:acceleration this) (float 0))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration)))

  Massive
  (apply-force [this force]
    (let [f (.get force)
          mf (PVector/div f (float (:mass this)))
          next-acceleration (PVector/add (:acceleration this) mf)]
      (assoc this :acceleration next-acceleration)))

  SelfAdapting
  (check-target [this target]
    (let [d (q/dist (.-x (:location this)) (.-y (:location this)) (.-x target) (.-y target))
          next-hit-target (< d (params :target-r))]
      (assoc this :hit-target next-hit-target)))

  (fitness [this target]
    (let [d (q/dist (.-x (:location this)) (.-y (:location this)) (.-x target) (.-y target))]
      (Math/pow (/ 1 d) 2)))

  Drawable
  (draw [this]
    (q/fill 200 100)
    (q/stroke 0)
    (q/push-matrix)
    (q/translate (.-x (:location this)) (.-y (:location this)))
    (let [theta (+ (.heading2D (:velocity this)) Math/PI 2)]
      (q/rotate theta))
    (q/rect-mode :center)

    ; Thrusters
    (q/fill (params :thrusters-color))
    (let [r (:r this)
          rh (/ r 2)
          r2 (* r 2)]
      (q/rect (* rh -1) r2 rh r)
      (q/rect rh r2 rh r)

      ; Rocket body
      (q/fill (params :rocket-color))
      (q/begin-shape :triangles)
      (q/vertex 0 (* r2 -1))        
      (q/vertex (* r -1) r2)
      (q/vertex r r2)    
      (q/end-shape))        
    (q/pop-matrix)))

(defn gen-rocket
  [& {:keys [id mass location velocity acceleration r fitness dna gene-counter hit-target] 
      :or {id "rx" mass 0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) 
           r 0 fitness 0 dna (random-DNA (params :lifetime)) gene-counter 0 hit-target false}}] 
  (Rocket. id mass location velocity acceleration r fitness dna gene-counter hit-target))

