(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll) (empty? (rest coll))))

(defn my-last [coll]
 (if (empty? (rest coll))
   (first coll)
   (my-last(rest coll))))

(defn max-element [a-seq]
  (if(empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (max-element (rest a-seq)) (first a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (and ((complement empty?) seq-1) ((complement empty?) seq-2))
       (seq-max (rest seq-1) (rest seq-2))
       (if (empty? seq-1)
         (str seq-2)
         (str seq-1))))

(defn longest-sequence [a-seq]
  )

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