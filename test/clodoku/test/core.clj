(ns clodoku.test.core
  (:use [clodoku.core] :reload)
  (:use [clojure.test]))

;; A few basic tests. Could do with loads more.

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

(def invalid-board
     ["1________"
      "_1_______"
      "_________"
      "_________"
      "_________"
      "_________"
      "_________"
      "_________"
      "_________"])
      
(deftest test-digit?
  (is (all digit? "0123456789"))
  ;; Be careful not to fall foul of de-Morgan's rule here :-p
  (is (all (complement digit?) "abcdefghijklmnopqrstuvwxyz")))

(deftest test-first-non-nil
  (is (first-non-nil [nil nil nil "STRING"]) "STRING")
  (is (first-non-nil ["HUSS", nil, nil, nil, "CUSS"]) "HUSS"))

(deftest test-sane-hash-map
  (is
   (sane-hash-map [ ["one", 1],
		    ["two", 2]])
   (hash-map "one" 1 "two" 2)))

(deftest test-enumerate
  (is (enumerate ["one" "two" "three"])
      [[0 "one"] [1 "two"] [2 "three"]]))

(deftest test-invalid-board
  (is (not (make-board invalid-board))))

(deftest test-board-no-search
  (is (board-to-vector (make-board eg-board1))
      ["534678912"
       "672195348"
       "198342567"
       "859761423"
       "426853791"
       "713924856"
       "961537284"
       "287419635"
       "345286179"]))

(deftest test-need-search
  (is (depth-first-search (make-board eg-board6))
			  ["342198576"
			   "961725384"
			   "857634912"
			   "173946258"
			   "498253167"
			   "526871493"
			   "639417825"
			   "285369741"
			   "714582639"]))
