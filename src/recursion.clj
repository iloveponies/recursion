(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max a-seq (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)]
      (if (pred? x)
        (cons x (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [first (first a-seq)]
    (if (or (empty? a-seq) (not (pred? first)))
      '()
      (cons first (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (let [first (first a-seq)]
    (cond (empty? a-seq) '()
          (pred? first) (my-drop-while pred? (rest a-seq))
          :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)]
    (or (and a-empty b-empty)
        (and
          (not a-empty)
          (not b-empty)
          (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (let [a-empty (empty? seq-1)
        b-empty (empty? seq-2)]
    (if (or a-empty b-empty)
      '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2))))))

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

