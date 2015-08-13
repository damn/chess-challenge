(ns chess-challenge.core
  (:require [data.grid2d :as g]))

;; Utilities

(defn assoc-vals
  "Associates indices in map m with value."
  [m indices value]
  (apply assoc m (interleave indices (repeat value))))

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

(def valid-pieces #{:king :queen :bishop :rook :knight})
(def occupied? valid-pieces)

(defn try-place
  "If square at position is empty and piece would not threaten any other pieces,
  places piece on board and threatened squares as threatened.
  Otherwise returns nil."
  [piece position grid]
  (let [cell (grid position)]
    (when (= cell :empty)
      (let [threatened-cells (piece->threatened-squares piece
                                                        position
                                                        [(g/width grid)
                                                         (g/height grid)])]
        (when (not-any? occupied? (map grid threatened-cells))
          (-> grid
              (assoc position piece)
              (assoc-vals threatened-cells :threatened)))))))

(defn already-calculated? [status grid]
  (contains? (:calculated-configurations status) grid))

(defn find-solutions*
  "The search function for recursion levels > 1.
  Meant to be called by 'find-solution', which is the starting point.
  Returns a new status hash:
  {:solutions, :solutions-count, :calculated-configurations}"
  [pieces grid count-only? status]
  (if (empty? pieces)
    (let [status (update status :solutions-count inc)]
      (if count-only?
        status
        (update status :solutions conj grid)))
    (loop [positions (g/posis grid)
           status status]
      (if (empty? positions)
        status
        (let [new-grid (try-place (first pieces) (first positions) grid)
              new-status (cond
                          ; position is not valid for piece: we do not go deeper here.
                          (nil? new-grid)
                          status

                          ; this configuration was already calculated: we do not go deeper here.
                          (already-calculated? status new-grid)
                          status

                          ; the piece could be placed on the grid and this configuration is not searched yet:
                          ; we go deeper here.
                          ; We continue with the other pieces and the new-grid contains the current piece placed on
                          ; the current position.
                          ; We also add this new configuration to calculated-configurations, to prevent duplicate effort.
                          :else
                          (find-solutions* (rest pieces)
                                           new-grid
                                           count-only?
                                           (update status :calculated-configurations conj new-grid)))]
          (recur (rest positions) new-status))))))

(defn find-solutions
  "Starts with the first piece of 'pieces' and places it on
  all possible positions on the grid.
  For each position, calls this function again with the remaining pieces.

  Returns a vector of [solutions solutions-count].

  If count-only? is true, then solutions will be empty."
  [pieces grid count-only?]
  {:pre [(seq pieces)
         (every? #(= % :empty) (g/cells grid))]}
  (let [first-piece (first pieces)
        max-posis (count (g/posis grid))
        print-progress #(println (int (* (/ % max-posis) 100)) "%")]
    (loop [positions (g/posis grid)
           progress 0
           status {:solutions []
                   :solutions-count 0
                   :calculated-configurations #{}}]
      (if-let [position (first positions)]
        (do
         (print-progress progress)
         (if-let [new-grid (try-place first-piece position grid)]
           (recur (rest positions)
                  (inc progress)
                  (find-solutions* (rest pieces) new-grid count-only? status))
           (throw (Error. "The first piece should be able to be placed on all board locations."))))
        [(:solutions status) (:solutions-count status)]))))

(defn print-grid [grid]
  (doseq [y (range (g/height grid))]
    (doseq [x (range (g/width grid))]
      (print (case (grid [x y])
               (:empty :threatened) "_"
               :king "K"
               :queen "Q"
               :bishop "B"
               :rook "R"
               :knight "N")))
    (println)))

(defn print-grids [grids]
  (doseq [grid grids]
    (print-grid grid)
    (println)))

(defn solve
  "Given a sequence of chess pieces and the dimensions width and height
  of the board, returns all configurations for which all of the pieces can be placed
  without threatening each other.
  A piece must be one of: :king :queen :bishop :rook :knight.

  Additonal argument is :count-only, for not returning all solutions as data (and printing them), but only returning
  the number of unique configurations.
  This is recommended for bigger board sizes, to save memory.
  For the following example, printing out over 3 million configurations to the console is not very useful.

  Example:
  (solve [:king :king :queen :queen :bishop :bishop :knight] 7 7 :count-only true)"
  [pieces width height & {:keys [count-only]}]
  {:pre [(every? valid-pieces pieces)
         (seq pieces)
         (integer? width)
         (pos? width)
         (integer? height)
         (pos? height)]}
  (let [empty-grid (g/create-grid width height (constantly :empty))
        [solutions solutions-count] (time (find-solutions pieces
                                                          empty-grid
                                                          count-only))]
    (println solutions-count " solutions found.")
    (if count-only
      nil
      (do (print-grids solutions)
          solutions))))

(defn -main [& args]
  (apply solve (map read-string args))
  (System/exit 0))
