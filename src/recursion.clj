(ns recursion)

(defn product [coll]
  (cond
   (empty? coll) 1
   :else (* (first coll)
            (product (rest coll)))))
  ;:-)

(defn singleton? [coll]
  (and
   ;(first coll)
   (empty? (rest coll))
   (not (empty? coll))))
  ;:-)

(defn my-last [coll]
  (first (reverse coll)))
  ;:-)

(defn max-element [a-seq]
  (last (sort a-seq)))
  ;:-)

(defn seq-max [seq-1 seq-2]
  (let [a (count seq-1)
        b (count seq-2)]
    (cond
     (== a b) seq-2
     (== (max a b) a) seq-1
     :else seq-2)))
  ;[:-])

(defn longest-sequence [a-seq]
  ;(fn [x] (max x (first a-seq)))
  (cond
   (empty? a-seq) nil
   :else (apply max-key count a-seq)))
  ;[:-])

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

