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

(def ^:dynamic calculated-configurations)
(def ^:dynamic solutions)
(def ^:dynamic solutions-count)
(def ^:dynamic count-only?)

(defn find-solutions*
  "The search function for recursion levels > 1.
  Meant to be called by 'find-solution', which is the starting point."
  [pieces grid]
  (let [next-one (first pieces)]
    (if-not next-one
      (do
       (when-not count-only?
         (swap! solutions conj grid))
       (swap! solutions-count inc))
      (doseq [position (g/posis grid)]
        (when-let [new-grid (try-place next-one position grid)]
          (when-not (contains? @calculated-configurations new-grid)
            (swap! calculated-configurations conj new-grid)
            (find-solutions* (rest pieces) new-grid)))))))

(defn find-solutions
  "Starts with the first piece of 'pieces' and places it on
  all possible positions on the grid.
  For each position, calls this function again with the remaining pieces.
  Is executed in parallel using clojure.core/pmap."
  [pieces grid]
  {:pre [(seq pieces)
         (every? #(= % :empty) (g/cells grid))]}
  (let [first-piece (first pieces)
        progress (atom 0)
        max-posis (count (g/posis grid))
        print-progress #(sprintln (int (* (/ (swap! progress inc) max-posis) 100)) "%")
        try-place (fn [position]
                    (print-progress)
                    (if-let [new-grid (try-place first-piece position grid)]
                      (find-solutions* (rest pieces) new-grid)
                      (throw (Error. "The first piece should be able to be placed on all board locations."))))]
    (dorun (pmap try-place (g/posis grid)))))

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

  (binding [solutions (atom [])
            solutions-count (atom 0)
            count-only? count-only
            calculated-configurations (atom #{})]

    (let [empty-grid (g/create-grid width height (constantly :empty))]
      (time (find-solutions pieces empty-grid))
      (println @solutions-count " solutions found.")
      (if count-only
        nil
        (do
         (print-grids @solutions)
         @solutions)))))

(defn -main [& args]
  (apply solve (map read-string args))
  (System/exit 0))
