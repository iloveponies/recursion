(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (let [rest-of-coll (rest coll)]
  (if (empty? rest-of-coll)
    (first coll)
    (my-last rest-of-coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn recursive-seq-max [seq-1 seq-2]
  (cond
   (singleton? seq-1) 2
   (singleton? seq-2) 1
   :else (recursive-seq-max (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (if (= (recursive-seq-max seq-1 seq-2) 1)
    seq-1
    seq-2))

(defn recursive-longest-sequence [a-seq length b-seq];TODO
  (let [first1 (first a-seq)
        rest-of (rest a-seq)
        count1 (count first1)]
  (if (singleton? a-seq)
    b-seq
    (if (< count1 length)
      (recursive-longest-sequence rest-of length b-seq)
      (recursive-longest-sequence rest-of count1 first1)))))

(defn longest-sequence [a-seq]
  recursive-longest-sequence [a-seq -1 nil])

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