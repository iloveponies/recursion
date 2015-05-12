(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (let [f (first coll)
          r (rest coll)]
      (if (empty? r)
        f
        (my-last r)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (empty? r)
        f
        (max f (max-element r))))))

(defn seq-max [seq-1 seq-2]
  (let [count-seq-1 (count seq-1)
        count-seq-2 (count seq-2)]
  (if (> count-seq-1 count-seq-2)
    seq-1
    seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (empty? r)
        f
        (seq-max f (longest-sequence r))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          r (rest a-seq)
          fil-r (my-filter pred? r)]
      (if (pred? f)
        (cons f fil-r)
        fil-r))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

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

