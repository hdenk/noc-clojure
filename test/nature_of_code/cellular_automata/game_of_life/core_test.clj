(ns nature-of-code.cellular-automata.game-of-life.core-test
  (:use clojure.test)
  (:require [nature-of-code.test-utils :as test-utils]
    [nature-of-code.cellular-automata.game-of-life.core :as game-of-life])
  (:import [nature_of_code.cellular_automata.game_of_life.core Board]))

(def test-params 
  {:size [300 200]
   :background 255
   :frame-rate 30
   :cell-w 8})

(deftest test-board
  (with-redefs [game-of-life/params test-params] 
    (testing 
      "step"
      (is 
        (= #{[2 1] [1 1] [0 1]} 
           (game-of-life/step #{[1 0] [1 1] [1 2]})))
      (is 
        (= '(#{[1 0] [1 1] [1 2]} #{[2 1] [1 1] [0 1]} #{[1 0] [1 1] [1 2]}) 
           (take 3 (iterate game-of-life/step #{[1 0] [1 1] [1 2]})))))))
