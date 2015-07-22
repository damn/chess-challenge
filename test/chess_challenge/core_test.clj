(ns chess-challenge.core-test
  (:require [data.grid2d :as g]
            [clojure.test :refer :all]
            [chess-challenge.core :refer :all]))

(deftest t-assoc-vals
  (is (= (assoc-vals {} (range 3) :foo)
         {0 :foo, 1 :foo, 2 :foo})))

(defn change-cells-threatened->empty [grid]
  (g/transform grid
               (fn [posi old-value]
                 (if (= old-value :threatened)
                   :empty
                   old-value))))

(defn same-boards? [solutions should-solutions]
  {:pre [(seq solutions)]}
  (let [solutions (set (map change-cells-threatened->empty solutions))
        empty-board (g/create-grid (g/width (first solutions))
                                   (g/height (first solutions))
                                   (constantly :empty))
        should-solutions (set
                          (map #(apply assoc empty-board (apply concat %))
                               should-solutions))]
    (is (= solutions should-solutions))))

(deftest first-pdf-example-board
  (let [solutions (solve [:king :king :rook] 3 3)
        should-solutions [{[0 0] :king [2 0] :king [1 2] :rook}
                          {[0 0] :king [0 2] :king [2 1] :rook}
                          {[2 2] :king [2 0] :king [0 1] :rook}
                          {[0 2] :king [2 2] :king [1 0] :rook}]]
    (same-boards? solutions should-solutions)))

(deftest second-pdf-example-board
  (let [solutions (solve [:rook :rook :knight :knight :knight :knight] 4 4)
        should-solutions [{[0 0] :rook, [1 1] :knight, [1 3] :knight, [2 2] :rook, [3 1] :knight, [3 3] :knight}

                          {[0 1] :rook, [1 0] :knight, [1 2] :knight, [2 3] :rook, [3 0] :knight, [3 2] :knight}

                          {[0 2] :rook, [1 1] :knight, [1 3] :knight, [2 0] :rook, [3 1] :knight, [3 3] :knight}

                          {[0 3] :rook, [1 0] :knight, [1 2] :knight, [2 1] :rook, [3 0] :knight, [3 2] :knight}

                          {[0 1] :knight, [0 3] :knight, [1 0] :rook, [2 1] :knight, [2 3] :knight, [3 2] :rook}

                          {[0 1] :knight, [0 3] :knight, [1 2] :rook, [2 1] :knight, [2 3] :knight, [3 0] :rook}

                          {[0 0] :knight, [0 2] :knight, [1 1] :rook, [2 0] :knight, [2 2] :knight, [3 3] :rook}

                          {[0 0] :knight, [0 2] :knight, [1 3] :rook, [2 0] :knight, [2 2] :knight, [3 1] :rook}]]
    (same-boards? solutions should-solutions)))
