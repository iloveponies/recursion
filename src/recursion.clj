(ns recursion)

(defn product [coll] (if (empty? coll) 1 (* (first coll) (product (rest coll)))))


(defn singleton? [coll] (if (and (empty? (rest coll)) (not= (first coll) nil)) true false))


(defn my-last [coll]  (if (empty? (rest coll) )
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq] (if (empty? a-seq)
  nil
  (reduce max a-seq)))


(defn seq-max [seq-1 seq-2]
  (cond
   (= (count seq-1) (count seq-2)) seq-2
    (= (count seq-1) (max (count seq-1) (count seq-2))) seq-1
    :else seq-2))


 (defn sup [a-seq]
  (cond
   (= (max (count ( first a-seq)) (count ( second a-seq)) (count (last a-seq))) (count ( first a-seq))) (first a-seq)
    (= (max (count ( first a-seq)) (count ( second a-seq)) (count (last a-seq))) (count ( second a-seq))) (second a-seq)
    :else (last a-seq)))

(defn longest-sequence [a-seq]
  (cond
   (= (count a-seq) 0) nil
    (= (count a-seq) 1) (first a-seq)
    :else (sup a-seq)))


(defn my-filter [pred? a-seq] (filter pred? a-seq))


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

