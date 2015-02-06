(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll)        false
   (empty? (rest coll)) true
   :else                false))

(defn my-last [coll]
  (cond
   (empty?     coll) nil
   (singleton? coll) (first coll)
   :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty?     a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else              (max (first a-seq)
                           (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty?     a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else              (seq-max (first a-seq)
                               (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)          false
   (== elem (first a-seq)) true
   :else                   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
                    true
    (empty? a-seq)  false
    (empty? b-seq)  false
    :else           (and (== (first a-seq) (first b-seq))
                         (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [x1 (first seq-1)
          x2 (first seq-2)
          fx (f x1 x2)]
      (cons fx
            (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (<= k 0) 1
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (<= n 0)      0
    (or (== n 1)
        (== n 2)) 1
    :else (let [fn-2 (fib (- n 2))
                fn-1 (fib (- n 1))]
            (+ fn-1 fn-2))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1)
                     what-to-repeat))))

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

