(ns nature-of-code.test-utils)

(defmacro dbg
  "print debug-infos to console"
  [x] 
  `(let 
     [x# ~x] 
     (println "dbg:" '~x "=" x#) x#)) 

(defn- square [n] 
  (* n n))

(defn close-to [a b] 
  (< (square(- a b)) 0.0001))

(defn pvector-close-to [v1 v2]
 (and 
    (close-to (.-x v1) (.-x v2)) 
    (close-to (.-y v1) (.-y v2)) 
    (close-to (.-z v1) (.-z v2))))
