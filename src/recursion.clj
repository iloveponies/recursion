(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (let [[x y] a-seq
          m (max x y)
          rest-seq (rest (rest a-seq))
          m-seq (vec (cons m (vec rest-seq)))]
      (max-element m-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (let [[x y] a-seq
          m (seq-max x y)
          rest-seq (rest (rest a-seq))
          m-seq (cons m rest-seq)]
      (longest-sequence m-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred?(rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (= elem (first a-seq))
    true
    (if (empty? (rest a-seq))
      false
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (pred? (first a-seq))
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))
    []))

(defn my-drop-while [pred? a-seq]
  (if (not (pred? (first a-seq)))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f v w]
  (cond
   (or (empty? v)
       (empty? w))
   []
   :else (cons (f (first v) (first w))
               (my-map f (rest v) (rest w)))))

(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power (dec k)))))

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

