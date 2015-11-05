(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (boolean (seq coll)) ;; idiomatic for collection emptiness
    (empty? (rest coll))))

(defn my-last [coll]
  (let [fst (first coll)
        rst (rest  coll)]
    (if (empty? rst)
      fst
      (my-last rst))))

(defn max-element [a-seq]
  (let [fst (first a-seq)
        rst (rest  a-seq)]
    (if (empty? rst)
      fst
      (max fst (max-element rst)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [fst (first a-seq)
        fst-count (count (first a-seq))
        rst (rest a-seq)]
    (if (empty? rst)
      fst
      (let [longest-sequence-rest (longest-sequence rst)]
        (if (> fst-count (count longest-sequence-rest))
          fst
          longest-sequence-rest)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [my-filter-others (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq)) 
        (cons (first a-seq) my-filter-others)
        my-filter-others))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or 
        (empty? a-seq)
        (not (pred? (first a-seq))))
    []
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [fst-a (first a-seq)
        fst-b (first b-seq)
        rst-a (rest a-seq)
        rst-b (rest b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (or  (empty? a-seq) (empty? b-seq)) false
      :else (and 
              (= fst-a fst-b)
              (seq= rst-a rst-b)))))

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

