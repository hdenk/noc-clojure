(ns nature-of-code.systems.particlesystem-forces.core
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :particle-color 127}) 

(defn size-x []
  (first (params :size)))

(defn size-y []
  (second (params :size)))

;;
;; Particle
;;

(defprotocol Stateful
  (next-state [this] "calc next state for the stateful object"))

(defprotocol Massiv
  (apply-force [this force] "apply force to the masive object"))

(defprotocol Expirable
  (expired? [this] "returns true when lifespan is over"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defrecord Particle [id mass location velocity acceleration lifespan]
  Stateful  
  (next-state [this]
    (let [next-location (PVector/add location velocity)
          next-velocity (PVector/add velocity acceleration)
          next-acceleration (PVector/mult acceleration (float 0))
          next-lifespan (- lifespan 2.0)]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))

  Massiv
  (apply-force [this force] 
    (let [f (.get force)
          mf (PVector/div f (float mass))
          next-acceleration (PVector/add acceleration mf)]
      (assoc this :acceleration next-acceleration)))

  Expirable
  (expired? [this] 
    (< lifespan 0.0))

  Drawable
  (draw [this]
    (q/stroke 0 lifespan)
    (q/stroke-weight 2)
    (q/fill (params :particle-color) lifespan)
    (q/ellipse (.-x location) (.-y location) 12 12)))

(defn gen-particle 
  [& {:keys [id mass location velocity acceleration lifespan] 
      :or {id "px" mass 0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) lifespan 0}}] 
  (map->Particle 
    {:id id 
     :mass mass 
     :location location 
     :velocity velocity 
     :acceleration acceleration 
     :lifespan lifespan})) 

;;
;; ParticleSystem
;;

(defn particles-next-state [particles]
  (map next-state particles))

(defn add-particle [particles origin]
  (conj 
    particles 
    (gen-particle :id (str "p" (count particles)) :mass 1.0 :location origin :velocity (PVector. (q/random -1.0 1.0) (q/random -2.0 0)) :lifespan 255)))

(defn remove-expired [particles]
  (remove expired? particles)) 

(defrecord ParticleSystem [origin particles]
  Stateful
  (next-state [this]
    (let [next-particles 
          (-> particles 
            (particles-next-state) 
            (add-particle origin) 
            (remove-expired))]
      (ParticleSystem. origin next-particles))) 

  Massiv
  (apply-force [this force]
    (let [next-particles (map #(apply-force % force) particles)]
      (ParticleSystem. origin next-particles))) 

  Drawable
  (draw [this]
    (dorun (map #(draw %) particles))))
;;
;; Main
;;

(def particle-system (atom (map->ParticleSystem {:origin (PVector. (/ (size-x) 2) (- (size-y) (* (size-y) 0.75))) :particles []})))

(defn setup []
  (q/frame-rate (params :frame-rate))
  (q/smooth))

(defn draw []
  ; draw Background
  (q/no-stroke)
  (q/fill 255) 
  (q/rect 0 0 (q/width) (q/height))

  ; draw Particles
  (draw @particle-system)

  ; update ParticleSystem to next-state
  (let [gravity (PVector. 0.0 0.1)]
    (swap! 
      particle-system 
      #(-> % 
         (apply-force gravity) 
         (next-state)))))

(q/defsketch particlesystem-forces 
  :title "Particle-System produces Particles that experience Gravity"
  :setup setup
  :draw draw
  :size (params :size))
