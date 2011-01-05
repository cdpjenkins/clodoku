 (ns sudoku.core
  (:use [clojure.set]))

(declare set-cell-value)

;; Util fns
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
	   


;;

(def all-cells (for [y (range 9)
		     x (range 9)]
		 [x y]))

;;

;  A region is either a row or a column or a 3x3 box
(def rows
  (for [row (range 9)]
    (for [x (range 9)]
      [x row])))

(def cols
  (for [col (range 9)]
    (for [y (range 9)]
      [col y])))

(defn get-box-cells [box]
  (let [top-x (* 3 (mod box 3))
	top-y (* 3 (quot box 3))]
    (for [y (range top-y (+ top-y 3))
	  x (range top-x (+ top-x 3))]
      [x y])))
(def boxes
  (for [box-num (range 9)]
    (get-box-cells box-num)))
(def regions (concat rows cols boxes))

; We need a mapping from cells to regions
(defn regions-for-cell [pos]
  (for [region regions :when (some #( = % pos) region)]
    region))

(def cell-to-regions
  (sane-hash-map
   (for [x (range 9)
	 y (range 9)]
     [[x y] (regions-for-cell [x y])])))

(defn peers-for-cell [pos]
  (let [regions (cell-to-regions pos)]
    (difference (set (apply union regions))
		(set [pos]))))

(def peers
  (sane-hash-map
    (for [x (range 9)
	  y (range 9)]
      [ [x y] (peers-for-cell [x y])])))

;; the board is a map from coord-pairs to Cells
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
  (let [cells-with-val (for [pos region :when (contains? (board pos) val)]
			 pos)]
    (if (= (count cells-with-val) 1)
      (set-cell-value board (first cells-with-val) val)
      board)))

(defn eliminate [board pos val]
  "Eliminate a the possibility specified by val from the cell specified by
   pos. Return the resulting board."
;;  (println "eliminate" board pos val)
  (if (not (contains? (board pos) val))
    board
    (let [new-possibles (difference (board pos) #{val})
      ;;      (println "new-possibles" new-possibles)
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
			(regions-for-cell pos))]
      board)))

(defn set-cell-value [board pos val]
  ;;  (println "set-cell-value" board pos val)
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
      board)))

(defn is-completed [board]
  (all identity (for [pos all-cells]
	    (= 1 (count (board pos))))))

(defn depth-first-search [board]
  (if (nil? board)
    nil
    (if (is-completed board)
      board
      (let [poses (sort (comparator #(< (count (board %1))
					(count (board %2))))
			(filter #(> (count (board %)) 1) all-cells))]
	(println poses)
	(println (first poses))
	board))))

;; TODO YOU ARE HERE.. finish implementing search

(def eg-board1
     ["53__7____"
      "6__195___"
      "_98____6_"
      "8___6___3"
      "4__8_3__1"
      "7___2___6"
      "_6____28_"
      "___419__5"
      "____8__79"])

(def eg-board6
     ["_____8__6"
      "9_17_53_4"
      "_____4_1_"
      "1__94__5_"
      "49__5__67"
      "_2__71__3"
      "_3_4_____"
      "2_53_97_1"
      "7__5_____"])

(defn cell-at [board coords]
  (board coords))

(defn print-board [board]
  (doseq [y (range 9)]
    (doseq [x (range 9)]
      (let [cell (board [x y])]
	    
	(print (if (not= (count cell) 1)
		 "_"
		 (first cell)))))
    (println)))
