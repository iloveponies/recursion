(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll) 
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    (max a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (map seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (when (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    (not (pred? (first a-seq))) (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (when (not (or (empty? seq-1) (empty? seq-2)))
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times) '()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) (reverse '())
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [seque (seq a-seq)]
    (if (empty? seque)
      (rest [0 '()])
      (cons seque (tails (rest seque))))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations
  ([a-seq]
   (if (empty? a-seq)
     '(())
     (rotations (seq a-seq) (count a-seq))))
  ([a-seq n]
   (if (= 0 n)
     '()
     (cons a-seq (rotations (concat (rest a-seq) [(first a-seq)] (dec n)))))))

(defn rotations-recur [seq-r seq-last seq-rest]
  (if (empty? seq-rest)
    seq-r
    (let [seq-next (conj (subvec seq-last 1) (first seq-last))]
      (recur (cons seq-next seq-r) seq-next (rest seq-rest)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-recur [] (vec a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-item (first a-seq)
          a-seq-next (next a-seq)
          item-freq (get freqs a-item 0)
          freqs-next (assoc freqs a-item (inc item-freq))]
      (recur freqs-next a-seq-next))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

 (defn un-frequencies-recur [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [a-map-item (first a-map)
          a-map-next (next a-map)
          item (key a-map-item)
          item-count (val a-map-item)
          item-repeated (repeat item-count item)
          a-seq-next (concat a-seq item-repeated)]
      (recur a-seq-next a-map-next))))

(defn un-frequencies [a-map]
  (un-frequencies-recur `() a-map))

(defn my-split-recur [vec-before non-zero a-seq-after]
  (if (or (zero? non-zero) (empty? a-seq-after))
    (vector (or (seq vec-before) `()) a-seq-after)
    (let [a-item (first a-seq-after)
          a-seq-after-next (next a-seq-after)
          a-vec-before-next (conj vec-before a-item)]
      (recur a-vec-before-next (dec non-zero) a-seq-after-next))))

(defn my-take [n coll]
  (get (my-split-recur [] n coll) 0))

(defn my-drop-recur [non-zero a-seq]
  (if (or (zero? non-zero) (empty? a-seq))
    (or a-seq `())
    (recur (dec non-zero) (next a-seq))))

(defn my-drop [n coll]
  (my-drop-recur n coll))

(defn halve [a-seq]
  (let [a-count (count a-seq)
        middle (int (/ a-count 2))]
   (my-split-recur [] middle a-seq)))

(defn seq-merge-recur [a-vec-res a-seq b-seq]
  (let [a-item (first a-seq)
        b-item (first b-seq)]
    (cond
      (empty? a-seq) (concat a-vec-res b-seq)
      (empty? b-seq) (concat a-vec-res a-seq)
      (<= a-item b-item) (recur (conj a-vec-res a-item) (next a-seq) b-seq)
      :else (recur (conj a-vec-res b-item) a-seq (next b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-recur [] a-seq b-seq))

(defn merge-sort-recur [a-seq b-seq]
  (let [a-sorted? (>= 1 (count a-seq))
        b-sorted? (>= 1 (count b-seq))
        a-seq-sorted (if a-sorted?
                       a-seq
                       (apply merge-sort-recur (halve a-seq)))
        b-seq-sorted (if b-sorted?
                       b-seq
                       (apply merge-sort-recur (halve b-seq)))]
    (seq-merge a-seq-sorted b-seq-sorted)))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply merge-sort-recur (halve a-seq))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

