(ns nature-of-code.systems.particlesystem-forces.core
  "Particle-System produces Particles that experience Gravity
	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q]
            [nature-of-code.math.vector :as mv]))

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :gravity [0.0 0.1]
   :lifespan 255
   :lifespan-dec-rate 2
   :particle-r 16
   :particle-color 127}) 

(defn size-x []
  (first (params :size)))

(defn size-y []
  (second (params :size)))

;;
;; Abstractions
;;

(defprotocol Mobile
  (move [this] "enter next motion state for the mobile object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Expirable
  (expired? [this] "returns true when lifespan is over"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

;;
;; Particle
;;

(defrecord Particle [id mass location velocity acceleration lifespan]
  Mobile  
  (move [this]
    (let [next-location (mv/add location velocity)
          next-velocity (mv/add velocity acceleration)
          next-acceleration (mv/multiply acceleration 0)
          next-lifespan (- lifespan (params :lifespan-dec-rate))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))

  Massiv
  (apply-force [this force] 
    (let [mf (mv/divide force (float mass))
          next-acceleration (mv/add acceleration mf)]
      (assoc this :acceleration next-acceleration)))

  Expirable
  (expired? [this] 
    (< lifespan 0))

  Drawable
  (draw [this]
    (q/stroke 0 lifespan)
    (q/stroke-weight 2)
    (q/fill (params :particle-color) lifespan)
    (q/ellipse (first location) (second location) (params :particle-r) (params :particle-r))))

(defn gen-particle 
  [& {:keys [id mass location velocity acceleration lifespan] 
      :or {id "px" mass 0 location [0 0] velocity [0 0] acceleration [0 0] lifespan 0}}] 
  (Particle. id mass location velocity acceleration lifespan)) 

;;
;; ParticleSystem
;;

(defn move-particles [particles]
  (map move particles))

(defn add-particle [particles origin]
  (conj 
    particles 
    (gen-particle :id (str "p" (count particles)) :mass 1.0 :location origin :velocity [(q/random -1.0 1.0) (q/random -2.0 0)] :lifespan (params :lifespan))))

(defn remove-expired [particles]
  (remove expired? particles)) 

(defrecord ParticleSystem [origin particles]
  Mobile
  (move [this]
    (let [ next-particles 
          (-> particles 
            (move-particles) 
            (add-particle origin) 
            (remove-expired))]
      (assoc this :particles next-particles))) 

  Massiv
  (apply-force [this force]
    (let [next-particles (map #(apply-force % force) particles)]
      (assoc this :particles next-particles))) 

  Drawable
  (draw [this]
    (dorun (map #(draw %) particles))))

;;
;; Sketch
;;

(def particle-system (atom (map->ParticleSystem {:origin [(/ (size-x) 2) (- (size-y) (* (size-y) 0.75))] :particles []})))

(defn setup-sketch []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  ; draw Particles
  (draw @particle-system)

  ; update ParticleSystem to next-state
  (let [gravity (params :gravity)]
    (swap! 
      particle-system 
      #(-> % 
         (apply-force gravity) 
         (move)))))

(defn run-sketch []
	(q/defsketch particlesystem-forces 
	  :title "Particle-System produces Particles that experience Gravity"
	  :setup setup-sketch
	  :draw draw-sketch
	  :size (params :size)))
