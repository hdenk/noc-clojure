(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params ^{:doc "DataStructure representing Params to customize the app"} 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :lifetime 200
   :mutation-rate 0.01
   :max-force 0.1
   :target-r 24
   :rocket-count 50
   :rocket-r 4
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

(defprotocol TargetAware
  (check-target [this target] "check if target-location is hit"))

(defprotocol FitnessAware
  (fitness [this target] "calculate fitness")) 

;;
;; DNA
;;

(defn random-gene [force]
  (let [angle (rand processing.core.PConstants/TWO_PI)
        rand-gene (PVector. (Math/cos angle) (Math/sin angle))]
    (.mult rand-gene (float (rand force))) ; Seiteneffekt
    rand-gene))

(defrecord DNA [max-force genes]
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
  (let [force (rand (params :max-force))
        genes (repeatedly lifetime #(random-gene force))]
    (DNA. (params :max-force) genes)))

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

  TargetAware
  (check-target [this target]
    (let [d (q/dist (.-x (:location this)) (.-y (:location this)) (.-x target) (.-y target))
          next-hit-target (< d (params :target-r))]
      (assoc this :hit-target next-hit-target)))

  FitnessAware
  (fitness [this target]
    (let [d (q/dist (.-x (:location this)) (.-y (:location this)) (.-x target) (.-y target))]
      (assoc this :fitness (Math/pow (/ 1 d) 2))))

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

;;
;; Population
;;

(defrecord Population [mutation-rate rockets mating-pool generation-count])

(defn gen-random-rockets [rocket-count]
  (reduce 
    #(conj %1 (gen-rocket
                :id (str "r" %2)
                :mass 1.0
                :location (PVector. (/ (q/width) 2) 50)
                :velocity (PVector. 0 0)
                :acceleration (PVector. 0 0)
                :r (params :rocket-r)
                :fitness 0
                :dna  (random-dna (params :lifetime))
                :gene-counter 0
                :hit-target false)) 
    []
    (range rocket-count)))

(defn gen-population 
  [mutation-rate rocket-count]
  (let [rockets (gen-random-rockets rocket-count)
        mating-pool []
        generation-count 0]
    (Population. mutation-rate rockets mating-pool generation-count))) 

(defn next-fitness [population]
  (let [next-rockets (reduce #(conj %1 (fitness %2 target)) [] (:rockets population))]
    (assoc population :rockets next-rockets))) 

(defn dup-rockets [rocket max-fitness]
  (let [norm-fitness (q/map-range (:fitness rocket) 0 max-fitness 0 1)
        n (* norm-fitness 100)]
    (repeat n rocket)))

(defn next-mating-pool [population]
  (let [rockets (:rockets population)
        max-fitness (apply max-key :fitness rockets)
        next-mating-pool (mapcat (reduce #(conj %1 (dup-rockets %2 max-fitness) [] rockets)))]
    (assoc population :mation-pool next-mating-pool)))

(defn combine-two-rockets [rocket1 rocket2]
  (let [dna1 (:dna rocket1)
        dna2 (:dna rocket2)
        new-dna (crossover dna1 dna2)]
    (gen-rocket :dna new-dna)))

(defn mutate-rocket [rocket mutation-rate]
  (mutate (:dna rocket) mutation-rate))  
  
(defn reproduce-rocket [index mating-pool mutation-rate]
  (let [pool-size (count mating-pool)
        rocket1-idx (rand-int pool-size)
        rocket2-idx (rand-int pool-size)
        rocket1 (get mating-pool rocket1-idx)
        rocket2 (get mating-pool rocket2-idx)
        new-rocket (combine-two-rockets rocket1 rocket2)]
    (mutate-rocket new-rocket mutation-rate)))

(defn next-reproduction [population]
  (let [mating-pool (:mating-pool population)
        next-rockets (reduce 
                       #(conj %1 (reproduce-rocket %2 mating-pool)) 
                       [] 
                       (range (count (:rockets population))))
        next-generation-count (inc (:generation-count population))]
    (assoc population :rockets next-rockets :generations next-generation-count)))

;;
;; World
;;

(defrecord World [population target life-count]

(defn gen-world []
  (let [population (gen-population (params :mutation-rate) (params :rocket-count)) 
        target (PVector. (/ (q/width) 2) (params :target-r))
        life-count 0]
    (World. population target life-count))) 

(def world (atom (gen-world)))  

;;
;; Sketch
;;

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  (let [population (:population @world)
        target (:target @world)
        life-count (:life-count @world)]
    (q/fill 0) 
    (q/ellipse (.-x target) (.-y target) (params :target-r) (params :target-r))
    
    (if (< life-count (params :lifetime))
      ; next step in current populations life
      (let [next-population (next-state population)
            next-life-count (inc life-count)]
        (swap! world assoc :population next-population :life-count next-life-count))
      ; next generation
      (let [next-population (-> population
                              (next-fitness)
                              (next-mating-pool)
                              (next-reproduction))]
          #(swap! world assoc :population next-population)))

    ; Display some info
    (q/fill 0) 
    (q/text (str "Generation #: " (:generation-count population)) 10 18)
    (q/text (str "Cycles left: " (- (param :lifetime) life-count)) 10 36))) 

(q/defsketch smart-rockets-superbasic 
  :title "Rockets adapt behavior to environment by applying genetic algorithm"
  :setup setup-sketch
  :draw draw-sketch
  :size (params :size))
