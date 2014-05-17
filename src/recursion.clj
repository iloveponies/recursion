(ns recursion)



;; Exercise 1
;; Write the function (product coll) that computes the product of a collection of values. The product of a, b and c is a∗b∗c.
;;
;; coll -> number
;; naive version
(defn product [coll]
  (if (empty? coll)
    ;; base case
    1
    ;; recursion
    (* (first coll)
       (product (rest coll)))))
;;
;; loop macro version
(defn product [coll]
  (loop [c    coll  ; temporary variable holding collection
         acc     1] ; temporary variable serving as an accumulator
    
    (if (empty? c)
      ;; base case
      acc
      ;; recursion
      (recur (* acc (first c)) (rest c)))))
;;
;; two arity version with tail call optimization without loop macro
(defn product
  ;; body with 1 arg (collection only)
  ([coll] (product coll 1))
  ;;
  ;; body with 2 args (collection and an accumulator)
  ([coll acc]
     (if (empty? coll)
       ;; base case
       acc
       ;; recursion
       (recur (rest coll) (* acc (first coll))))))
;;
(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!



(defn singleton? [coll]
  :-)

(defn my-last [coll]
  :-)

(defn max-element [a-seq]
  :-)

(defn seq-max [seq-1 seq-2]
  [:-])

(defn longest-sequence [a-seq]
  [:-])

(defn my-filter [pred? a-seq]
  [:-])

(defn sequence-contains? [elem a-seq]
  :-)

(defn my-take-while [pred? a-seq]
  [:-])

(defn my-drop-while [pred? a-seq]
  [:-])

(defn seq= [a-seq b-seq]
  :-)

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

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
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

