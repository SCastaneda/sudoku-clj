(ns sudoku)
(use 'clojure.java.io)

(defn in? [coll elm]
  (some #(= elm %) coll))

(defn get-field [board x y]
  (nth (nth board x) y))

(defn is-valid?
  "Checks if the area is valid.
  Area is a list, transformed from a row, column, or square.
  The list is first stripped of zeros, then checked for duplicates.
  Checking for duplicates is done by sorting the list and checking for equality
  of the first 2 elements in the list recursively"
  [area]
  (let [sorted (sort (filter (comp not zero?) area))]
    (loop [_ sorted]
      (if (seq _)
        (when (#(or (= 1 (count %))
                    (apply (comp not =) %))
               (take 2 _))
          (recur (rest _)))
        true))))

(defn get-square-coordinates
  "Given one field's coordinates on a board,
  returns the coordinates of entire square on the board.
  board is assumed to be a 9x9 and a square is a 3x3"
  [x y]
  (let [begx (* (quot x 3) 3)
        endx (+ begx 3)
        begy (* (quot y 3) 3)
        endy (+ begy 3)]
    (for [row (range begx endx) col (range begy endy)]
         [row col])))

(defn get-square [board x y]
  (let [fields (get-square-coordinates x y)]
    (into []
      (map #(get-field board (first %) (second %))
        fields))))

(defn get-row [board row-num]
  (nth board row-num))

(defn get-col [board col-num]
  (into [] (map #(nth % col-num) board)))

(defn get-all-possible [board x y]
  "given the current board state, get all possible
  numbers at the given coordinates(x, y).
  This is simply done by filtering all existing numbers
  of this coordinates row, col, and square."
  (let [poss (range 1 10)]
    (filter #(and
                (not (in? (get-row board x) %))
                (not (in? (get-col board y) %))
                (not (in? (get-square board x y) %)))
        poss)))

(defn is-solvable? [board]
  (let [check-rows-n-cols
        (fn [bd n]
          (cond
            (< n 0) true
            (and  (is-valid? (get-row bd n))
                  (is-valid? (get-col bd n)))
            (recur bd (- n 1))
            :else false))

        check-squares
        (fn [bd squares]
          (cond
            (empty? squares) true
            :else
              (let [square (first squares) x (first square) y (second square)]
                (if (is-valid? (get-square bd x y))
                    (recur bd (rest squares))
                    false))))]
    (and
      (check-rows-n-cols board (- (count board) 1))
      (check-squares board (for [x (range 3) y (range 3)] [(* x 3) (* y 3)])))))

(defn line-to-array [line]
  (into [] (map #(Integer/parseInt %) (seq (-> line (.split " "))))))

(defn split-by-nl [file-contents]
  (into [] (seq (-> file-contents (.split "\n")))))

(defn get-board
  ([ls]
   (get-board ls []))
  ([ls res]
   (cond
     (empty? ls) res
     true (get-board (rest ls) (conj res (line-to-array (first ls)))))))

(defn print-board [board]
  (doseq [row board]
    (println row)))

(defn attempt
  ([board]
   (attempt board 0 0))
  ([board x y]
   (if (= 0 (get-field board x y))
       (let [poss (get-all-possible board x y)]
         (attempt board x y poss))
       (cond
         (and (= x 8) (= y 8)) board ;; we're done
         :else (if (< y 8)
                 (attempt board x (inc y))
                 (attempt board (inc x) 0)))))
  ([board x y poss]
   (cond
     (empty? poss) false ;; return false to start backtracking
     (and (= x 8) (= y 8)) (assoc-in board [x y] (first poss))
     :else
       (let [att (assoc-in board [x y] (first poss))
             p (if (< y 8)
                   (attempt att x (inc y))
                   (attempt att (inc x) 0))]
            (if-not p ;; returnd false... try the next possible number
                    (attempt board x y (rest poss))
                    p))))) ;; returned a board... keep sending it back up

(defn get-board-from-file [filename]
  (-> filename slurp split-by-nl get-board))

(defn solve [board]
  (if (is-solvable? board)
      (print-board (attempt board))
      (println "Board not solvable")))

(defn solve-file[filename]
  (let [board (get-board-from-file filename)]
    (solve board)))

;;(solve-file "/Users/sam/projects/clojure/sudoku.txt")
