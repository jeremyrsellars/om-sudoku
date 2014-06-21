
(ns om-sudoku.sudoku)

(defn- bubble-max-key [k coll]
  "Move a maximal element of coll according to fn k (which returns a number)
   to the front of coll."
  (let [max (apply max-key k coll)]
    (cons max (remove #(identical? max %) coll))))

(defn union
  "Return a set that is the union of the input sets"
  {:added "1.0"}
  ([] #{})
  ([s1] s1)
  ([s1 s2]
     (if (< (count s1) (count s2))
       (reduce conj s2 s1)
       (reduce conj s1 s2)))
  ([s1 s2 & sets]
     (let [bubbled-sets (bubble-max-key count (conj sets s2 s1))]
       (reduce into (first bubbled-sets) (rest bubbled-sets)))))

(defn difference
  "Return a set that is the first set without elements of the remaining sets"
  {:added "1.0"}
  ([s1] s1)
  ([s1 s2]
     (if (< (count s1) (count s2))
       (reduce (fn [result item]
                   (if (contains? s2 item)
                     (disj result item)
                     result))
               s1 s1)
       (reduce disj s1 s2)))
  ([s1 s2 & sets]
     (reduce difference s1 (conj sets s2))))


(def board identity)

(def sample-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))


(def empty-board
  (board [[0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0]]))

(def b sample-board)



(def all-values #{1 2 3 4 5 6 7 8 9})
(def block-rows 3)
(def block-cols 3)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[r c] coord]
    (set (board r))))

(defn col-values [board coord]
  (let [[r c] coord]
    (set (map #(get % c) board))))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn block-values [board coord]
  (let [[r c] coord
        start-row (- r (mod r block-rows))
        start-col (- c (mod c block-cols))
        rows  (range start-row (+ start-row block-rows))
        cols  (range start-col (+ start-col block-cols))
        coords (for [rb rows
                     cb cols]
                 [rb cb])]
    (->> coords
         (map #(value-at board %))
         set)))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [used-in-row   (row-values board coord)
        used-in-col   (col-values board coord)
        used-in-block (block-values board coord)
        used          (union used-in-row used-in-col used-in-block)
        remaining     (difference all-values used)]
    remaining)))

(defn filled? [board]
  (->>
   (for [row board
         cell row]
      cell)
   (reduce min 1)
   ((complement zero?))))

(defn rows [board]
  (map set board))

(defn- valid-sets [sets]
  (= sets
     (repeat (count all-values) all-values)))

(defn valid-rows? [board]
  (valid-sets (rows board)))

(defn cols [board]
  (map #(set (col-values board [0 %])) (range 9)))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn blocks [board]
  (for [rb (range block-rows)
        cb (range block-cols)]
    (set
     (block-values board
                   [(* rb block-rows) (* cb block-cols)]))))

(defn valid-blocks? [board]
  (valid-sets (cols board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (->> (for [r (range (count all-values))
             c (range (count all-values))]
         [r c])
       (filter #(zero? (value-at board %)))
       first))

(defn solve [board]
  (letfn [(guess-next-move-board [coord value]
            (set-value-at board coord value))
          (guess-next-move-boards []
            (let [coord (find-empty-point board)]
              (map #(solve (guess-next-move-board coord %)) (valid-values-for board coord))))]
    (cond (valid-solution? board) board
          (filled? board)         nil
          :else                   (some identity (guess-next-move-boards)))))
