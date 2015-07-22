(ns chess-challenge.core
  (:require [data.grid2d :as g]))

;; Utilities

(def printer (agent nil))

(defmacro sprintln
  "Synchronized println using an agent."
  [& text]
  `(do
    (send printer (fn [a#] (println ~@text)))
    nil))

(defn assoc-vals
  "Associates indices in map m with value."
  [m indices value]
  (apply assoc m (interleave indices (repeat value))))

;; Collecting and printing solutions

(defn print-cell [cell]
  (print (case cell
           (:empty :threatened) "_"
           :king "K"
           :queen "Q"
           :bishop "B"
           :rook "R"
           :knight "N")))

(defn print-grid [grid]
  (doseq [y (range (g/height grid))]
    (doseq [x (range (g/width grid))]
      (print-cell (grid [x y])))
    (println)))

(def ^:dynamic solutions) ; a collection of solution grids

(defn add-to-solutions [grid]
  (swap! solutions conj grid))

(defn print-grids [grids]
  (doseq [grid grids]
    (print-grid grid)
    (println)))

;; Main logic

(defn in-bounds? [[x y] [width height]]
  (and (>= x 0) (< x width) (>= y 0) (< y height)))

(defn piece->threatened-squares
  [piece piece-location board-size]
  {:pre [(in-bounds? piece-location board-size)]}
  (let [in-bounds? #(in-bounds? % board-size)
        move (partial mapv +)
        move-piece (partial move piece-location)]
    (case piece
      :king (let [moves (for [x [-1 0 1]
                              y [-1 0 1]
                              :when (not= [x y] [0 0])]
                          [x y])]
              (->> moves (map move-piece) (filter in-bounds?)))

      :bishop (apply concat
                     (for [direction [[1 1] [1 -1] [-1 1] [-1 -1]]]
                       (loop [squares []
                              current piece-location]
                         (let [square (move current direction)]
                           (if (in-bounds? square)
                             (recur (conj squares square) square)
                             squares)))))

      :rook (let [[px py] piece-location
                  [width height] board-size]
              (remove (partial = piece-location)
                      (concat
                       (for [x (range 0 width)] [x py])
                       (for [y (range 0 height)] [px y]))))

      :knight (let [steps (for [first-step [2 -2]
                                second-step [1 -1]]
                            [first-step second-step])]
                (->> (concat steps (map (comp vec reverse) steps))
                     (map move-piece)
                     (filter in-bounds?)))

      :queen (concat
              (piece->threatened-squares :bishop piece-location board-size)
              (piece->threatened-squares :rook piece-location board-size)))))

(defn try-place
  "If square at position is empty and piece would not threaten any other pieces,
  places piece on board,
  marking its square as occupied and threatened squares as threatened
  otherwise returns nil."
  [piece position grid]
  (let [cell (get grid position)]
    (when (= cell :empty)
      (let [threatened-cells (piece->threatened-squares piece
                                                        position
                                                        [(g/width grid)
                                                         (g/height grid)])]
        (when (every? #{:empty :threatened}
                      (map grid threatened-cells))
          (-> grid
              (assoc position piece)
              (assoc-vals threatened-cells :threatened)))))))

(def ^:dynamic calculated-configurations)

(defn find-solutions
  "Starts with the first piece and tries to place on all
  possible positions in the grid, for each position calls this function again,
  with the next piece in list.
  When all pieces placed, a solution has been found."
  [pieces grid]
  (let [next-one (first pieces)]
    (if-not next-one
      (add-to-solutions grid)
      (doseq [position (g/posis grid)]
        (when-let [new-grid (try-place next-one position grid)]
          (when-not (contains? @calculated-configurations new-grid)
            (swap! calculated-configurations conj new-grid)
            (find-solutions (rest pieces) new-grid)))))))

(defn find-solutions-1 ; TODO duplicated method
  "Starts with the first piece and tries to place on all
  possible positions in the grid, for each position calls this function again,
  with the next piece in list.
  When all pieces placed, a solution has been found."
  [pieces grid]
  (let [next-one (first pieces)
        progress (atom 0)
        max-posis (count (g/posis grid))
        ]
    (if-not next-one
      (add-to-solutions grid) ; unreachable code
      (dorun (pmap
              (fn [position]
                (sprintln "Progress: ~"
                          (int (* (/ (swap! progress inc) max-posis)
                                  100))
                          "%")
                (when-let [new-grid (try-place next-one position grid)] ; place the first one.
                  (find-solutions (rest pieces) new-grid)))
              (g/posis grid))))))

;; each square of grid can have one of these values:
;; :empty, :threatened, occupied (-> :king, queen, ...)

;; useful to work with the results in the REPL
(def last-results (atom nil))

(defn solve
  "Given a sequence of chess pieces and the dimensions width and height
  of the board, returns all configurations for which all of the pieces can be placed
  without threatening each other.
  A piece must be one of: :king :queen :bishop :rook :knight."
  [pieces width height]
  {:pre [(every? #{:king :queen :bishop :rook :knight} pieces)
         (seq pieces)
         (integer? width)
         (pos? width)
         (integer? height)
         (pos? height)]}
  (binding [solutions (atom [])
            calculated-configurations (atom #{})]
    (let [empty-grid (g/create-grid width height (fn [position] :empty))]
      (find-solutions-1 pieces empty-grid)
      ;(println (count @solutions) " solutions found.")
      ;(swap! solutions distinct)
      (println (count @solutions) " distinct solutions found.")
      ;(print-grids @solutions)
      ;(reset! last-results [@solutions @calculated-configurations])
      @solutions
      )))

;(time (solve [:king :king :queen :queen :bishop :bishop :knight] 3 3))
; 23 752 solutions
; 40 sekunden
; with parallelizing: 20 seconds
; with flagged configurations: 5 sec.!
; 7x7 only possible with flagged configurations.

; TODO lein run arguments
; TODO tests (vom blatt)
