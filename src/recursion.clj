(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [count1 (count seq-1)
        count2 (count seq-2)]
    (if (> count1 count2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (cons first-elem (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (cons first-elem (my-take-while pred? (rest a-seq)))
      :else [])))

(defn my-drop-while [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (my-drop-while pred? (rest a-seq))
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails-old [a-seq]
  (if (or (empty? a-seq) (and (singleton? a-seq) (empty? (first a-seq))))
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [seq-1 seq-2]
  (let [new-rotation (concat seq-2 seq-1)]
    (if (empty? seq-2)
      '()
      (cons new-rotation (rotations-helper (concat seq-1 [(first seq-2)]) (rest seq-2))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ['()]
    (rotations-helper '() a-seq)))

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

