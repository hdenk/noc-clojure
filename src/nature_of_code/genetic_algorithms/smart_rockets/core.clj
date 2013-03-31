(ns nature-of-code.genetic-algorithms.smart-rockets.core
  "Rockets adapt behavior to environment by applying genetic algorithm
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

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
   :default-mutation-rate 0.03
   :increased-mutation-rate 0.1
   :max-force 1.0 
   :target-r 20
   :obstacle-w 200
   :obstacle-h 20
   :rocket-count 100 
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

; TODO find better names for the abstraction(s)

(defprotocol Mobile
  (move [this] "moves the mobile Object"))

(defprotocol Massive
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Spatial
  (containso? [this spot] "returns true if the spatial object contains the spot"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defprotocol Genetic   
  (crossover [this partner] "produce new DNA by mixing genes of two individuals")
  (mutate [this mutation-rate] "mutate based on probability"))

;;
;; DNA
;;

(defn random-gene [max-force]
  (let [angle (rand processing.core.PConstants/TWO_PI)
        rand-gene (vector (Math/cos angle) (Math/sin angle))]
    (mv/multiply rand-gene (rand max-force))
    rand-gene))

(defrecord DNA [max-force genes]
  Genetic
  (crossover [dna partner-dna]
    (let [crossover (rand-int (count (:genes dna)))
          child-genes (into 
                        [] 
                        (concat 
                          (first (split-at crossover (:genes dna))) 
                          (second (split-at crossover (:genes partner-dna)))))]
      (assoc dna :genes child-genes)))

  (mutate [dna mutation-rate]
    (let [mutated-genes (into 
                          []
                          (map    
                            #(if (< (rand) mutation-rate)
                               (random-gene (params :max-force))   
                               %) 
                            (:genes dna)))]
      (assoc dna :genes mutated-genes)))) 

(defn gen-dna 
  [& {:keys [maxforce genes] 
      :or {maxforce 0.0 genes []}}] 
  (DNA. maxforce genes)) 

(defn random-dna [lifetime]
  (let [genes (vec (repeatedly lifetime #(random-gene (params :max-force))))]
    (gen-dna :max-force (params :max-force) :genes genes)))

;;
;; Obstacle
;;

(defrecord Obstacle [location w h]
  Spatial
  (containso? [obstacle spot]
    (let [ox (first (:location obstacle))
          oy (second (:location obstacle))
          ow (:w obstacle)
          oh (:h obstacle)
          sx (first spot)
          sy (second spot)]
      (and (> sx ox) 
           (< sx (+ ox ow))
           (> sy oy)
           (< sy (+ oy oh)))))

  Drawable
  (draw [obstacle]
    (q/stroke 0)
    (q/fill 175)
    (q/stroke-weight 2)
    (q/rect-mode :corner)
    (let [x (first (:location obstacle))
          y (second (:location obstacle))
          w (:w obstacle)
          h (:h obstacle)]
      (q/rect x y w h))))

(defn gen-obstacle 
  [& {:keys [location w h] 
      :or {location [0 0] w 0 h 0}}] 
  (Obstacle. location w h)) 

;;
;; Rocket
;;

(defn next-motion-state [rocket]
  (let [next-location (mv/add (:location rocket) (:velocity rocket))
        next-velocity (mv/add (:velocity rocket) (:acceleration rocket))
        next-acceleration (mv/multiply (:acceleration rocket) (float 0))]
    (assoc rocket :location next-location :velocity next-velocity :acceleration next-acceleration)))

(defrecord Rocket [id mass location velocity acceleration r fitness dna gene-index min-distance hit-obstacle hit-target]
  Mobile 
  (move [rocket]
    (if-not (or (:hit-target rocket) (:hit-obstacle rocket))
      (let [dna (:dna rocket)
            genes (:genes dna)
            gene-index (:gene-index rocket)
            force (get genes gene-index)
            next-gene-index (mod (inc gene-index) (count genes))]
        (-> rocket 
            (apply-force force)
            (next-motion-state)
            (assoc :gene-index next-gene-index)))
      rocket))

  Massive
  (apply-force [rocket force]
    (let [mf (mv/divide force (:mass rocket))
          next-acceleration (mv/add (:acceleration rocket) mf)]
      (assoc rocket :acceleration next-acceleration)))

  Drawable
  (draw [rocket]
    (q/fill 200 100)
    (q/stroke 0)
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (first (:location rocket)) (second (:location rocket)))
    ; Draw a triangle rotated in the direction of velocity
    (let [theta (+ (mv/heading-2d (:velocity rocket)) (/ Math/PI 2))]
      (q/rotate theta))

    (let [r (:r rocket)
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
  [& {:keys [id mass location velocity acceleration r fitness dna gene-index min-distance hit-obstacle hit-target] 
      :or {id "rx" mass 1.0 location [0 0] velocity [0 0] acceleration [0 0] 
           r (params :rocket-r) fitness 0 dna [] gene-index 0 min-distance Integer/MAX_VALUE hit-obstacle false hit-target false}}] 
  (Rocket. id mass location velocity acceleration r fitness dna gene-index min-distance hit-obstacle hit-target))

(defn fitness [rocket]
  (let [gene-index (:gene-index rocket)
        finish-index (if (> gene-index 0) gene-index (params :lifetime))
        fq (/ 1.0 (* finish-index (:min-distance rocket)))
        fexp (Math/pow fq 2)
        next-fitness (if (:hit-obstacle rocket) 
                       (* fexp 0.05)
                       (if (:hit-target rocket)
                         (* fexp 2.0)
                         fexp))]     
      (assoc rocket :fitness next-fitness)))

(defn check-target [rocket target]
  (let [d (q/dist (first (:location rocket)) (second (:location rocket)) (first target) (second target))
        next-hit-target (< d (- (params :target-r) 2))
        next-min-distance (min (:min-distance rocket) d)]
    (assoc rocket :hit-target next-hit-target :min-distance next-min-distance)))

(defn check-obstacles [rocket obstacles]
  (let [next-hit-obstacle (reduce 
                            #(or %1 (containso? %2 (:location rocket))) 
                            false obstacles)]
    (assoc rocket :hit-obstacle next-hit-obstacle)))

;;
;; Population
;;

(defrecord Population [mutation-rate rockets mating-pool generation-count])

(defn gen-population
  [& {:keys [mutation-rate rockets mating-pool generation-count] 
      :or {mutation-rate 0.0 rockets [] mating-pool [] generation-count 0}}] 
  (Population. mutation-rate rockets mating-pool generation-count))

(defn gen-random-rockets [rocket-count]
  (into [] 
        (map
          #(gen-rocket
             :id (str "r" %)
             :location (vector (/ (size-x) 2) (- (size-y) 20))
             :dna  (random-dna (params :lifetime)))
          (range rocket-count))))

(defn check-and-move-rocket [obstacles target rocket]
  (if-not (or (:hit-target rocket) (:hit-obstacle rocket))
    (-> rocket
      (check-obstacles obstacles)
      (check-target target)
      (move))
    rocket))

(defn next-rockets-state [population obstacles target]
  (let [next-rockets (into [] (map (partial check-and-move-rocket obstacles target) (:rockets population)))]
   (assoc population :rockets next-rockets)))

(defn calc-rockets-fitness [population target]
  (let [next-rockets (into [] (map fitness (:rockets population)))]
    (assoc population :rockets next-rockets)))

(defn dup-rockets [rocket max-fitness]
  (let [norm-fitness (q/map-range (:fitness rocket) 0 max-fitness 0 1)
        n (int (* norm-fitness 100))]
    (dbg (str (:id rocket) ":" n ":" (:fitness rocket) ":" (:hit-obstacle rocket) ":" (:hit-target rocket) ":" (:gene-index rocket) ":" (:min-distance rocket)))
    (repeat n rocket)))

(defn gen-mating-pool [rockets max-fitness]
  (vec (apply concat (map #(dup-rockets % max-fitness) rockets))))

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
                :location (vector (/ (size-x) 2) (- (size-y) 20))
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
        (map  
          #(reproduce-rocket % mating-pool mutation-rate) 
          (range rockets-count))))

(defn adjust-mutation-rate [population]
  (let [next-mutation-rate (if (> (count (filter :hit-target (:rockets population))) 0)
                             (dbg (params :default-mutation-rate)) 
                             (dbg (params :increased-mutation-rate)))]
    (assoc population :mutation-rate next-mutation-rate))) 

(defn next-generation [population]
  (let [mutation-rate (:mutation-rate population)
        mating-pool (:mating-pool population)
        rockets-count (count (:rockets population))
        next-rockets (reproduce-rockets rockets-count mating-pool mutation-rate)
        next-generation-count (inc (:generation-count population))]
    (assoc population :rockets next-rockets :generation-count next-generation-count)))

(defn draw-rocket [rocket]
  (when-not (:hit-obstacle rocket)
     (draw rocket)
     rocket))
  
(defn draw-population [population]
  (dorun (map draw-rocket (:rockets population))))

;;
;; World
;;

(defrecord World [population obstacles target life-count])

(defn gen-world 
  [& {:keys [population obstacles target life-count] 
      :or {population nil obstacles [] target [0 0] life-count 0}}] 
  (World. population obstacles target life-count)) 

(def world (atom {}))  

;;
;; Sketch
;;

(defn gen-obstacles [w h]
  (let [x (- (/ (size-x) 2) (/ w 2))
        y (- (/ (size-y) 2) (/ h 2))
        location (vector x y)]
    (vector (gen-obstacle :location location :w w :h h))))  

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth)

  ; initialize world
  (let [rockets (gen-random-rockets (params :rocket-count))
        population (gen-population :mutation-rate (params :mutation-rate) :rockets rockets)
        obstacles (gen-obstacles (params :obstacle-w) (params :obstacle-h))
        target (vector (/ (size-x) 2) (params :target-r))]
    (swap! world (constantly (gen-world :population population :obstacles obstacles :target target :life-count 0)))))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  (let [population (:population @world)
        target (:target @world)
        obstacles (:obstacles @world)
        life-count (:life-count @world)]
    ; draw target
    (q/fill 0) 
    (q/ellipse (first target) (second target) (params :target-r) (params :target-r))

    ; draw rockets 
    (draw-population (:population @world))

    ; draw obstacles
    (dorun (map draw obstacles))

    ; state-progression 
    (if (< life-count (params :lifetime))
      ; next step in current generations life 
      (let [next-population (next-rockets-state population obstacles target)
            next-life-count (inc life-count)]
        (swap! world assoc :population next-population :life-count next-life-count))
      ; lifetime over: next generation
      (let [next-population (-> population
                                (calc-rockets-fitness target)
                                (populate-mating-pool)
                                (adjust-mutation-rate)
                                (next-generation))]
        (swap! world assoc :population next-population :life-count 0)))

    ; Display some info
    (q/fill 0) 
    (q/text (str "Generation #: " (:generation-count population)) 10 18)
    (q/text (str "Cycles left: " (- (params :lifetime) life-count)) 10 36))) 

(defn mouse-pressed [] 
  (swap! world assoc :target (vector (q/mouse-x) (q/mouse-y))))

(defn run-sketch []
  (q/defsketch smart-rockets-superbasic 
    :title "Rockets adapt behavior to environment by applying genetic algorithm"
    :setup setup-sketch
    :draw draw-sketch
    :mouse-pressed mouse-pressed
    :size (params :size)))
