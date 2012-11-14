(ns nature-of-code.forces.fluidresistance.view-graph
  "A view-graph is a tree of GraphNodes"
  (:require [quil.core :as q]))

(defprotocol Node
  "A graph node, which can draw itself
   and its children"
  (draw [this state]))

(defrecord GraphNode [children child-state-fn draw-fn]
  Node
  (draw [this state]
    (draw-fn state)
    (let [child-state (child-state-fn state)]
      (doseq [x children]
        (draw x child-state)))))

(defn make-graph-node [children child-state-fn draw-fn]
  (GraphNode. children child-state-fn draw-fn))
