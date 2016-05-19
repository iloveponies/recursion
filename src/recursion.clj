(ns recursion)

;Ex1 Write the function (product coll) that computes the product of a collection of values. The product of aa, bb and cc is a∗b∗ca∗b∗c.
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;Ex2 Write down the evaluation of (product [1 2 4]) like we did for sum above. This exercise does not give any points and you do not need to return it.
#_(
    (product [1 2 3])
    = (product (conj (conj (conj [] 1) 2) 3))
    => (* 3 (product (conj (conj [] 1) 2)))
    => (* 3 (* 2 (product (conj [] 1))))
    => (* 3 (* 2 (* 1 (product []))))                       ; (empty? []) is true, so (product [] ;=> 1
    => (* 3 (* 2 (* 1 1)))
    => (* 3 (* 2 1))
    => (* 3 2)
    => 6
    )

;Ex3 Write the function (singleton? coll) which returns true if the collection has only one element in it and false otherwise.
; This is a very useful helper function in the remainder of this chapter.
;Do not use count as it can be expensive on long sequences. This function is not recursive.
(defn singleton? [coll]
  (and
    (boolean (not-empty coll))
    (empty? (rest coll))
    ))


;Ex4 Write (my-last a-seq) that computes the last element of a sequence.
(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

;Ex5 Write the function (max-element a-seq) that computes returns the maximum element in a-seq or nil if a-seq is empty?
;You can use the function (max a b) that returns the greater of a and b.
(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

;Ex6 Write the function (seq-max seq-1 seq-2) that returns the longer one of seq-1 and seq-2.
; This is a helper for the next exercise. You do not need to use recursion here. It is okay to use count.
(defn seq-max [seq-1 seq-2]
  (let [cnt1 (count seq-1)
        cnt2 (count seq-2)]
    (if (= cnt2 (max-element [cnt1 cnt2]))
      seq-2
      seq-1)))

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

