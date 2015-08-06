(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (not (empty? coll))
    (empty? (rest coll))
    false))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (not (empty? coll))
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if-not (empty? a-seq)
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
    (if-not (empty? a-seq)
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2))
     '()
   :else
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k)
     1
   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
     0
   (== 1 n)
     1
   :else
     (+ (fib (- n 1))
        (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0)
     '()
   :else
     (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to)
     '()
   :else
     (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq)
     (cons '() a-seq)
   :else
     (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
   (empty? a-seq)
     (cons '() '())
   :else
     (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key1     (first a-seq)
          newfreqs (if (contains? freqs key1)
                     (assoc freqs key1 (inc (get freqs key1)))
                     (assoc freqs key1 1))]
      (my-frequencies-helper newfreqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (concat (repeat (val (first a-map)) (key (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== 0 n)
          (== 0 (count coll)))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== 0 (count coll))
    '()
    (if (== 0 n)
      (cons (first coll)
            (my-drop 0 (rest coll)))
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    (vector (my-take middle a-seq)
            (my-drop middle a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   :else
     (if (<= (first a-seq) (first b-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
       (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq)
     '()
   (singleton? a-seq)
     a-seq
   :else
     (seq-merge (merge-sort (seq (first (halve a-seq))))
                (merge-sort (seq (first (rest (halve a-seq))))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

