(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
   (* (first coll)
	(product (rest coll)))))

;(* 1 (product (cons 2 (cons 4))))
;(* 1 (* 2 (product (cons 4))))
;(* 1 (* 2 (* 4 (product [])))) empty => 1
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll))
	(empty? (rest coll))))

(defn my-last [coll]
  (if (not (empty? (rest coll)))
    (my-last (rest coll))
    (first coll)))

(defn max-element [a-seq]
  (if (not (empty? (rest a-seq)))
    (max (first a-seq) (max-element (rest a-seq)))
    (first a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
   	 seq-1
	 seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
   (first a-seq)
    (if (empty? a-seq)
     nil
     (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
     (cons (first a-seq) (my-filter pred? (rest a-seq)))
     (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
	true
	(sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
     a-seq
     (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (if (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
     a-seq)))

(defn seq= [a-seq b-seq]
(cond 
 (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq))
   (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  (if (== k 0)
   1
   (* n (power n( (- k 1))))))

(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
     1
     (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
 	(cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
         []))

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (empty? coll)
     []
  (if (== n 0)
     []
    (cons (first coll) (my-take (- n 1) (rest coll))))))

(defn my-drop [n coll]
  (if (empty? coll)
    []
   (if (== n 0)
    coll
    (my-drop (- n 1) (rest coll)))))

(defn halve [a-seq]
  (cons (my-take (int (/ (count a-seq) 2)) a-seq)
  (cons (my-drop (int (/ (count a-seq) 2)) a-seq) [])))

(defn seq-merge [a-seq b-seq]
 (sort (concat a-seq b-seq)))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

