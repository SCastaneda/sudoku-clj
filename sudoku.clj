(use 'clojure.java.io)


(defn get-field [board x y]
  (nth (nth board x) y)
  )

(defn is-valid? 
  "Checks if the area is valid. 
  Area is a list, transformed from a row, column, or square.
  The list is first stripped of zeros, then checked for duplicates.
  Checking for duplicates is done by sorting the list and checking for equality
  of the first 2 elements in the list recursively"
  [area]
  (let [sorted (sort (filter #(not (zero? %)) area))]
    ((fn [l]
      (let [n (count l)]
        (cond 
          (<= n 1) true
          (= n 2) (not (= (first l) (second l)))
          (> n 2) (if (= (first l) (second l)) 
                      false
                      (recur (rest l)))
          )
        )
      ) sorted)
    )
  )

(defn get-square-coordinates 
  "Given one fields coordinates in a board, 
  returns the coordinates of entire square on the board
  board is assumed to be a 9x9 and a square is a 3x3"
  [x y] 
  (let [begx (* (quot x 3) 3) 
        endx (+ begx 3) 
        begy (* (quot y 3) 3) 
        endy (+ begy 3)] 
    (for [row (range begx endx) col (range begy endy)] 
          [row col]
      ) 
    )
  )

(defn get-square [board x y]
  (let [fields (get-square-coordinates x y)] 
    (into [] 
      (map #(get-field board (first %) (second %)) 
        fields
        )
      )
    )
  )

(defn get-row [board row-num]
  (nth board row-num)
  )

(defn get-col [board col-num]
  (into [] (map #(nth % col-num) board))
  )


(defn is-solvable? [board]
  (let [rows-n-cols 
    ((fn [bd n]
    (cond
      (< n 0) true
      (and  (is-valid? (get-row bd n))
            (is-valid? (get-col bd n))
            ) (recur bd (- n 1))
      true false
            
      )
    ) board (- (count board) 1))

    squares
    ((fn [bd squares]
      (cond
        (empty? squares) true
        true 
          (let [square (first squares) x (first square) y (second square)]
            (if (is-valid? (get-square bd x y))
                (recur bd (rest squares))
                false
              )
            )
        
        )
      ) board (for [x (range 3) y (range 3)] [(* x 3) (* y 3)]))

    ]
    (and rows-n-cols squares)
    )
  )


(defn line-to-array [line]
  (into [] (map #(Integer/parseInt %) (seq (-> line (.split " ")))))
  )

(defn split-by-nl [file-contents]
  (into [] (seq (-> file-contents (.split "\n"))))
  )

(defn get-board
  ([ls]
    (get-board ls [])
    )
  ([ls res]
    (cond
      (empty? ls) res
      true (get-board (rest ls) (conj res (line-to-array (first ls))))
      )
    )
  )

(defn print-board [board]
  (cond
    (empty? board) nil
    true (do (println (first board)) (print-board (rest board)))
    )
  )



(defn is-valid? 
  "Checks if the area is valid. 
  Area is a list, transformed from a row, column, or square.
  The list is first stripped of zeros, then checked for duplicates.
  Checking for duplicates is done by sorting the list and checking for equality
  of the first 2 elements in the list recursively"
  [area]
  (let [sorted (sort (filter #(not (zero? %)) area))]
    ((fn [l]
      (let [n (count l)]
        (cond 
          (<= n 1) true
          (= n 2) (not (= (first l) (second l)))
          (> n 2) (if (= (first l) (second l)) 
                      false
                      (recur (rest l)))
          )
        )
      ) sorted)
    )
  )

(defn attempt 
  ([board]
    (attempt board 0 0 1))
  ([board x y number]
    (if (= 0 (get-field board x y))
        (let [att (assoc-in board [x y] number)]
          (if (and  (is-valid? (get-row att x)) 
                    (is-valid? (get-col att y)) 
                    (is-valid? (get-square att x y))
                )
              (let [p 
                (cond 
                  (and  (= x 8) (= y 8)) att
                  true  (if  (< y 8) 
                            (attempt att x (+ y 1) 1)
                            (attempt att (+ x 1) 0 1)
                          )

                )]
                (if (false? p) ;; we're backtracking, try next number
                  (if (< number 9) 
                    (recur board x y (+ 1 number))
                    false ;; failed -- keep backtracking
                    )
                  p
                  )
                )
              (if (< number 9) 
                  (recur board x y (+ 1 number))
                  false ;; failed, not possible
                )
            )
          )
          (cond 
            (and (= x 8) (= y 8)) board ;; we're done
            true (if (< y 8) 
                  (attempt board x (+ y 1) 1)
                  (attempt board (+ x 1) 0 1)
                  )
            )

      )
    )
  )

(defn get-board-from-file [filename]
  (get-board (split-by-nl (slurp filename)))
  )

(defn solve [filename]
  (let [board (get-board-from-file filename)]
    (if (is-solvable? board)
        (print-board (attempt board))
        (println "Board not solvable")
      )
    
    )
  )
