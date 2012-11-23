(ns nature-of-code.systems.particlesystem-polymorphism.core
  (:require [quil.core :as q])
  (:import [processing.core PVector]))

(def params 
  {:size [600 400]
   :background 255
   :frame-rate 30
   :lifespan 255
   :lifespan-dec-rate 2
   :particle-r 16
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

(def particle-next-state
  {:next-state (fn [this] 
    (let [next-location (PVector/add (:location this) (:velocity this))
          next-velocity (PVector/add (:velocity this) (:acceleration this))
          next-acceleration (PVector/mult (:acceleration this) (float 0))
          next-lifespan (- (:lifespan this) (params :lifespan-dec-rate))]
      (assoc this :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))})

(def particle-apply-force
  {:apply-force (fn [this force] 
    (let [f (.get force)
          mf (PVector/div f (float (:mass this)))
          next-acceleration (PVector/add (:acceleration this) mf)]
      (assoc this :acceleration next-acceleration)))})

(def particle-expired?
  {:expired? (fn [this] 
    (< (:lifespan this) 0.0))})

(def circular-particle-draw
  {:draw (fn [this] 
    (q/stroke 0 (:lifespan this))
    (q/stroke-weight 2)
    (q/fill (params :particle-color) (:lifespan this))
    (q/ellipse (.-x (:location this)) (.-y (:location this)) (params :particle-r) (params :particle-r)))})

(def squared-particle-draw
  {:draw (fn [this]
    (q/stroke 0 (:lifespan this))
    (q/stroke-weight 2)
    (q/fill (params :particle-color) (:lifespan this))
    (q/rect-mode :center)
    (q/push-matrix)
    (q/translate (.-x (:location this)) (.-y (:location this)))
    (let [theta (q/map-range (.-x (:location this)) 0 (size-x) 0 (* Math/PI 2))]
      (q/rotate theta))
    (q/rect 0 0 12 12) ; TODO 12 -> params
    (q/pop-matrix))})

(defrecord CircularConfetti [id mass location velocity acceleration lifespan])

(extend CircularConfetti
  Stateful 
  particle-next-state

  Massiv
  particle-apply-force
  
  Expirable
  particle-expired?

  Drawable
  circular-particle-draw)

(defrecord SquaredConfetti [id mass location velocity acceleration lifespan])

(extend SquaredConfetti
  Stateful 
  particle-next-state

  Massiv
  particle-apply-force
  
  Expirable
  particle-expired?

  Drawable
  squared-particle-draw)

(defn gen-particle 
  [& {:keys [id mass location velocity acceleration lifespan] 
      :or {id "px" mass 0 location (PVector. 0 0) velocity (PVector. 0 0) acceleration (PVector. 0 0) lifespan 0}}] 
  (if (> (rand 1) 0.5)
    (CircularConfetti. id mass location velocity acceleration lifespan)
    (SquaredConfetti. id mass location velocity acceleration lifespan)))
     

;;
;; ParticleSystem
;;

(defn next-particles-state [particles]
  (map next-state particles))

(defn add-particle [particles origin particle-count]
  (conj 
    particles 
    (gen-particle :id (str "p" particle-count) :mass 1.0 :location origin :velocity (PVector. (q/random -1.0 1.0) (q/random -2.0 0)) :lifespan (params :lifespan))))

(defn remove-expired [particles]
  (remove expired? particles)) 

(defrecord ParticleSystem [origin particles particle-count]
  Stateful
  (next-state [this]
    (let [ next-particles 
          (-> (:particles this) 
            (next-particles-state) 
            (add-particle (:origin this) (:particle-count this)) 
            (remove-expired))]
      (ParticleSystem. (:origin this) next-particles (inc (:particle-count this))))) 

  Massiv
  (apply-force [this force]
    (let [next-particles (map #(apply-force % force) (:particles this))]
      (ParticleSystem. (:origin this) next-particles (:particle-count this)))) 

  Drawable
  (draw [this]
    (dorun (map #(draw %) (:particles this)))))
;;
;; Sketch
;;

(def particle-system 
  (atom 
    (map->ParticleSystem 
      {:origin (PVector. (/ (size-x) 2) (- (size-y) (* (size-y) 0.75))) 
       :particles []
       :particle-count 0})))

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
  (let [gravity (PVector. 0.0 0.1)]
    (swap! 
      particle-system 
      #(-> % 
         (apply-force gravity) 
         (next-state)))))

(q/defsketch particlesystem-forces 
  :title "Particle-System produces Particles that experience Gravity"
  :setup setup-sketch
  :draw draw-sketch
  :size (params :size))
