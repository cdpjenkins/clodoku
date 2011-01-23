(ns clodoku.core
  (:use [clojure.set])
  (:gen-class))

(import
 '(java.io FileReader BufferedReader))


(declare set-cell-value)
(declare search-at-cell)

;; TODOs
;;
;; - solve Sudokus with multiple valid solutions
;; - complete rename to clodoku
;; - add capability to generate puzzles
;; - bit more error checking in -main function
;; - deal more intelligently with a board that takes too long to search.
;;   Perhaps iterative deepening search would be smarter or some arbitrary
;;   limit on depth that stops us taking forever

;;
;; A sudoku board is a 9x9 grid of cells.
;; Each cell is a set of possible integer values for that cell. If the set
;; has just one member then the value of that cell is known. If the set has
;; zero members then we have a contradiction and the board is invalid.
;; In addition, the board is divided into 9 rows, 9 columns and 9 boxes.
;; Each of these, we call a region. Therefore, each cell is a member of
;; precisely three regions (one row, one column and one box). A cell is a
;; peer of another cell if those two cells are members of the same region.
;; Two cells that are peers cannot have the same value.
;;
;; In Clojure, the board is represented as a hashmap from [x y] coord pairs to
;; a set of integers.
;;

;; Util fns
(defn first-non-nil [l]
  (if (empty? l)
    nil
    (if (nil? (first l))
      (first-non-nil (rest l))
      (first l))))

(defn sane-hash-map [key-val-pairs]
  (reduce (fn [the-map [key val]]
	    (assoc the-map key val))
	  {}
	  key-val-pairs))

(defn enumerate [l]
  (map vector l (range (count l))))

(defn digit? [c]
  (let [i (int c)]
    (and (<= i 57)
	 (>= i 48))))

(defn in? [val l]
  (some #( = % val) l))

(defn all [p l]
  "Returns true iff p returns true for all l"
  (if (empty? l)
    true
    (if (p (first l))
      (all p (rest l))
      false)))

(defn readlines [^BufferedReader b]
  "Return a list of the lines read from a BufferedReader"
  (let [line (.readLine b)]
    (if (nil? line)
      nil
      (cons line (readlines b)))))

;; List of all positions on the board from [0 0] to [8 8]
(def all-cells (for [y (range 9)
		     x (range 9)]
		 [x y]))

(defn value-at [board pos]
  "The value of the cell at pos, represented as a char, or _ if the value
   is not known"
  (let [cell (board pos)]
    (if (not= (count cell) 1)
      \_
      (char (+ (first cell) 48)))))

(def regions
  (let [rows (for [row (range 9)]
	       (for [x (range 9)]
		 [x row]))
	cols (for [col (range 9)]
	       (for [y (range 9)]
		 [col y]))
	boxes (for [box-num (range 9)]
		(let [top-x (* 3 (mod box-num 3))
		      top-y (* 3 (quot box-num 3))]
		  (for [y (range top-y (+ top-y 3))
			x (range top-x (+ top-x 3))]
		    [x y])))]
     (concat rows cols boxes)))

;; Mapping from cells to regions
(def cell-to-regions
  (sane-hash-map
   (for [pos all-cells]
     [pos (for [region regions :when (some #( = % pos) region)]
	    region)])))

;; Mapping from cells to the peers of that cell
(def peers
  (sane-hash-map
    (for [pos all-cells]
      [ pos (let [regions (cell-to-regions pos)]
	      (difference (set (apply union regions))
			  (set [pos])))])))

;; the board is a map from coord-pairs to cells
(defn parse-board [cell-char-array]
  "Takes an array of strings and outputs a seq of 3-tuples [x y value] for
   all positions that already have a value"
  (filter (complement nil?)
	  (for [[row y] (enumerate cell-char-array)
		[c   x] (enumerate row)]
	    (if (digit? c)
	      [x y (- (int c) 48)]
	      nil))))

(defn make-board [cell-char-array]
  (let [board (sane-hash-map
	       (for [[row y] (enumerate cell-char-array)
		     [c   x] (enumerate row)]
		 [ [x y]
		   (if (= c \_)
		     (set (range 1 10))
		     (set (range 1 10)))]))]
    (reduce (fn [board [x y val]]
	      (set-cell-value board [x y] val))
	    board
	    (parse-board cell-char-array))))

(defn scan-region-for-single-choice [board region val]
  (if (nil? board)
    nil
    (let [cells-with-val (for [pos region :when (contains? (board pos) val)]
			   pos)]
      (if (= (count cells-with-val) 1)
	(set-cell-value board (first cells-with-val) val)
	board))))

(defn eliminate [board pos val]
  "Eliminate a the possibility specified by val from the cell specified by
   pos. Return the resulting board. If this causes the cell to only have
   one possibility left then we the the cell's value to that single possibility.
   If this leaves a region with only one cell that could contain the value that
   we just eliminated then we set that cell to that value."
  (if (nil? board)
    nil
    (if (not (contains? (board pos) val))
      board
      (let [new-possibles (difference (board pos) #{val})
	    board (condp = (count new-possibles)
		      0 nil    ; contradiction
		      1 (set-cell-value (assoc board pos new-possibles)
					pos
					(first new-possibles))
		      (assoc board pos new-possibles))
	    
	    ;; find any regions that now only have one possible cell that could
	    ;; contain val
	    board (reduce (fn [board region]
			    (scan-region-for-single-choice board region val))
			  board
			  (cell-to-regions pos))]
	board))))

(defn set-cell-value [board pos val]
  "Set the cell specified by pos to the value specified by val. Eliminate that
   value from all peers of the cell."
  (if (nil? board)
    nil
    (if (and (= (count (board pos)) 1)
	     (not= (first (board pos)) val))
      nil ; contradiction
      (let [other-possibles (vec (difference (board pos)
					     #{val}))
	    ;; eliminate all other possibles from this cell
	    board (reduce (fn [b possible-to-eliminate]
			    (eliminate b pos possible-to-eliminate))
			  board
			  other-possibles)
	    ;; eliminate this val from all peers
	    board (reduce (fn [board peer]
			    (eliminate board peer val))
			  board
			  (peers pos))]
	board))))

(defn is-completed [board]
  "True iff all cells have precisely one possibility ie all cells have known
   values"
  (all identity (for [pos all-cells]
	    (= 1 (count (board pos))))))

(defn depth-first-search [board]
  "Perform a depth first search, returning the first complete (and valid!)
   board found"
  (if (nil? board)
    nil
    (if (is-completed board)
      board
      (let [pos (first (sort (comparator #(< (count (board %1))
					     (count (board %2))))
			     (filter #(> (count (board %)) 1) all-cells)))
	    possibilities (board pos)
	    result (first-non-nil (for [poss possibilities]
				    (set-cell-value board pos poss)))]
	(if (not (nil? result))
	  (depth-first-search result)
	  nil)))))			 

(defn board-to-vector [board]
  "Returns a 2d vector representing the board in row-major order. Known cell
   values are represented as a char of that value. Unkown cell values are
   represented a _"
  (vec
    (for [y (range 9)]
      (apply str (for [x (range 9)]
		   (value-at board [x y]))))))

(defn print-board [board]
  "Print a representation of the known values of the board to stdout"
  (doseq [row (board-to-vector board)]
    (println row)))

(defn -main [filename]
  (let [reader (BufferedReader. (FileReader. filename))
	solution (depth-first-search (make-board (readlines reader)))]
    (print-board solution)))
