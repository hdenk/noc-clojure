(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.core
  (:require [clojure.core.reducers :as r])
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(def params ^{:doc "DataStructure representing Params to customize the app"} 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :lifetime 200
   :mutation-rate 0.01
   :max-force 1.0 
   :target-r 24
   :rocket-count 500 
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

(defprotocol Mobile
  (move [this] "calc next motion-state for the movable object")
  (next-motion-state [this target] "proceed to next motion state"))

(defprotocol Massive
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Genetic 
  (crossover [this partner] "produce new DNA by mixing genes of two individuals")
  (mutate [this mutation-rate] "mutate based on probability"))

(defprotocol FitnessAware 
  (fitness [this target] "calculate fitness")) 

(defprotocol TargetAware 
  (check-target [this target] "check if target-location is hit"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; DNA
;;

(defn random-gene [force]
  (let [angle (rand processing.core.PConstants/TWO_PI)
        rand-gene (PVector. (Math/cos angle) (Math/sin angle))]
    (.mult rand-gene (float (rand force))) ; Seiteneffekt
    rand-gene))

(defrecord DNA [max-force genes]
  Genetic
  (crossover [this partner-dna]
    (let [crossover (rand-int (count (:genes this)))
          child-genes (into 
                        [] 
                        (concat 
                          (first (split-at crossover (:genes this))) 
                          (second (split-at crossover (:genes partner-dna)))))]
      (assoc this :genes child-genes)))

  (mutate [this mutation-rate]
    (let [mutated-genes (into []
                              (map    
                                #(if (< (rand) mutation-rate)
                                   (random-gene 0.1)
                                   %) 
                                (:genes this)))]
      (assoc this :genes mutated-genes)))) 

(defn random-dna [lifetime]
  (let [force (rand (params :max-force))
        genes (vec (repeatedly lifetime #(random-gene force)))]
    (DNA. (params :max-force) genes)))

;;
;; Rocket
;;

(defrecord Rocket [id mass location velocity acceleration r fitness dna gene-index hit-target]
  Mobile 
  (move [this]
    (let [next-location (PVector/add (:location this) (:velocity this))
          next-velocity (PVector/add (:velocity this) (:acceleration this))
          next-acceleration (PVector/mult (:acceleration this) (float 0))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration)))

  (next-motion-state [this target]
    (if-not (:hit-target this)
      (let [dna (:dna this)
            genes (:genes dna)
            gene-index (:gene-index this)
            force (get genes gene-index)
            next-gene-index (mod (inc gene-index) (count genes))]
        (-> this
          (apply-force force)
          (move)
          (check-target target)
          (assoc :gene-index next-gene-index)))
      this))

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
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (.-x (:location this)) (.-y (:location this)))
    ; Draw a triangle rotated in the direction of velocity
    (let [theta (+ (.heading2D (:velocity this)) (/ Math/PI 2))]
      (q/rotate theta))

    (let [r (:r this)
          rh (/ r 2)
          r2 (* r 2)]
      ; Thrusters
      (q/fill (params :thrusters-color))
      (q/rect (* rh -1) r2 rh r)
      (q/rect rh r2 rh r)

      ; Rocket body
      (q/fill (params :rocket-color))
      (q/begin-shape :triangles)
      (q/vertex 0 (* r2 -1))        
      (q/vertex (* r -1) r2)
      (q/vertex r r2)    
      (q/end-shape :close)) ; processing.core.PConstants/CLOSE
    (q/pop-matrix)))

(defn gen-rocket
  [& {:keys [id mass location velocity acceleration r fitness dna gene-counter hit-target] 
      :or {id "rx" mass 1.0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) 
           r (params :rocket-r) fitness 0 dna nil gene-counter 0 hit-target false}}] 
  (Rocket. id mass location velocity acceleration r fitness dna gene-counter hit-target))

;;
;; Population
;;

(defrecord Population [mutation-rate rockets mating-pool generation-count])

(defn gen-random-rockets [rocket-count]
  (into [] 
    (map
      #(gen-rocket
         :id (str "r" %)
         :location (PVector. (/ (size-x) 2) (- (size-y) 20))
         :dna  (random-dna (params :lifetime)))
      (range rocket-count))))

(defn gen-population 
  [mutation-rate rocket-count]
  (let [rockets (gen-random-rockets rocket-count)
        mating-pool []
        generation-count 0]
    (Population. mutation-rate rockets mating-pool generation-count))) 

(defn next-motion-state [population target]
  (let [next-rockets (map 
                       #(next-motion-state % target) 
                       (:rockets population))] 
    (assoc population :rockets next-rockets)))

(defn calc-fitness [population target]
  (let [next-rockets (into []
                           (map
                             #(fitness % target) 
                             (:rockets population)))]
    (assoc population :rockets next-rockets))) 

(defn dup-rockets [rocket max-fitness]
  (let [norm-fitness (q/map-range (:fitness rocket) 0 max-fitness 0 1)
        n (int (* norm-fitness 100))]
    (repeat n rocket)))

(defn gen-mating-pool [rockets max-fitness]
  (vec 
    (apply 
      concat 
      (map 
        #(dup-rockets % max-fitness) rockets))))

(defn populate-mating-pool [population]
  (let [rockets (:rockets population)
        max-fitness (:fitness (apply max-key :fitness rockets))
        next-mating-pool (gen-mating-pool rockets max-fitness)]
    (assoc population :mating-pool next-mating-pool)))

(defn combine-two-rockets [rocket-index rocket1 rocket2]
  (let [dna1 (:dna rocket1)
        dna2 (:dna rocket2)
        new-dna (crossover dna1 dna2)]
    (gen-rocket :id (str "r" rocket-index)
                :location (PVector. (/ (size-x) 2) (- (size-y) 20))
                :dna new-dna)))

(defn mutate-rocket [rocket mutation-rate]
  (let [next-dna (mutate (:dna rocket) mutation-rate)]
    (assoc rocket :dna next-dna)))  

(defn reproduce-rocket [rocket-index mating-pool mutation-rate]
  (let [pool-size (count mating-pool)
        rocket1-idx (rand-int pool-size)
        rocket2-idx (rand-int pool-size)
        rocket1 (get mating-pool rocket1-idx)
        rocket2 (get mating-pool rocket2-idx)
        new-rocket (combine-two-rockets rocket-index rocket1 rocket2)]
    (mutate-rocket new-rocket mutation-rate)))

(defn reproduce-rockets [rockets-count mating-pool mutation-rate]
  (into [] 
    (r/map  ; clojure.core.reducers/map -> fork-join
      #(reproduce-rocket % mating-pool mutation-rate) 
      (range rockets-count)))) 

(defn next-generation [population]
  (let [mutation-rate (:mutation-rate population)
        mating-pool (:mating-pool population)
        rockets-count (count (:rockets population))
        next-rockets (reproduce-rockets rockets-count mating-pool mutation-rate)
        next-generation-count (inc (:generation-count population))]
    (assoc population :rockets next-rockets :generation-count next-generation-count)))

(defn draw-population [population]
  (dorun (map draw (:rockets population))))

;;
;; World
;;

(defrecord World [population target life-count])

(defn gen-world []
  (let [population (gen-population (params :mutation-rate) (params :rocket-count)) 
        target (PVector. (/ (size-x) 2) (params :target-r))
        life-count 0]
    (World. population target life-count))) 

(def world (atom {}))  

;;
;; Sketch
;;

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)
  (swap! world (constantly (gen-world))))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  (let [population (:population @world)
        target (:target @world)
        life-count (:life-count @world)]
    ; draw target
    (q/fill 0) 
    (q/ellipse (.-x target) (.-y target) (params :target-r) (params :target-r))

    ; draw rockets 
    (draw-population (:population @world))

    (if (< life-count (params :lifetime))
      ; next step in current populations life
      (time
      (let [next-population (next-motion-state population target)
            next-life-count (inc life-count)]
        (swap! world assoc :population next-population :life-count next-life-count)))
      ; next generation
      (let [next-population (-> population
                              (calc-fitness target)
                              (populate-mating-pool)
                              (next-generation))]
        (swap! world assoc :population next-population :life-count 0)))

    ; Display some info
    (q/fill 0) 
    (q/text (str "Generation #: " (:generation-count population)) 10 18)
    (q/text (str "Cycles left: " (- (params :lifetime) life-count)) 10 36))) 

(defn mouse-pressed [] 
  (swap! world assoc :target (PVector. (q/mouse-x) (q/mouse-y))))

(q/defsketch smart-rockets-superbasic 
  :title "Rockets adapt behavior to environment by applying genetic algorithm"
  :setup setup-sketch
  :draw draw-sketch
  :mouse-pressed mouse-pressed
  :size (params :size))
