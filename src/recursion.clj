(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (*
     (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (let [the-first (first coll)
        the-rest (rest coll)]
    (if (empty? the-rest)
      the-first
      (my-last the-rest))))


(defn max-element [a-seq]
  (let [[x & xs] a-seq]
    (if (empty? xs)
      x
      (max x (max-element xs)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (<= c1 c2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)
          pred-x (pred? x)
          xs (rest a-seq)
          filtered-xs (my-filter pred? xs)]
      (if pred-x
        (cons x filtered-xs)
        filtered-xs))))

(defn sequence-contains? [elem a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
     (empty? a-seq) false
     (= elem x)true
     :else (sequence-contains? elem xs))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [x (first a-seq)
          pred-x (pred? x)
          xs (rest a-seq)
          take-rest (my-take-while pred? xs)]
      (cond
       pred-x (cons x take-rest)
       :else '()))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (and
          (== (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+
          (fib (dec n))
          (fib (dec (dec n))))))

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

