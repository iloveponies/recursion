(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) ((complement empty?) coll)))

(defn my-last [coll]
  (let [remainders (rest coll)]
    (if (singleton? coll)
      (first coll)
      (if (or (singleton? remainders) (empty? coll))
        (first remainders)
        (my-last remainders)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (my-last (sort a-seq))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq) nil
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [current (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? current) (cons current (my-take-while pred? (rest a-seq) ))
      :else ())))

(defn my-drop-while [pred? a-seq]
  (let [current (first a-seq)
        others (rest a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? current) (my-drop-while pred? others)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [firsts-are-equal (= (first a-seq) (first b-seq))
        both-seqs-are-empty (and (empty? a-seq) (empty? b-seq))]
    (cond
      both-seqs-are-empty true
      firsts-are-equal (seq= (rest a-seq) (rest b-seq))
      :else false)))

(defn my-map [f seq-1 seq-2]
  (let [either-seq-is-empty (or (empty? seq-1) (empty? seq-2))]
    (if either-seq-is-empty
      ()
      (cons
        (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

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

