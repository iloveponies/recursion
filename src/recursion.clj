(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element2 [a-seq max-fn]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max-fn (first a-seq) (max-element2 (rest a-seq) max-fn))))

(defn max-element [a-seq]
  (max-element2 a-seq max))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (max-element2 a-seq seq-max))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

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
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) ((complement pred?) (first a-seq)))
      a-seq
      (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (or (empty? a-seq) (empty? b-seq))
      (and (empty? a-seq) (empty? b-seq))
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (conj '() a-seq)
    (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (singleton? a-seq)
      a-seq
    (singleton? (rest a-seq))
      (concat a-seq (reverse a-seq))
    :else
      '()))

(defn my-frequencies-helper [freqs a-seq]
  (let [elem (first a-seq)
        elem-freq (get freqs elem 0)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
        (assoc freqs elem (inc elem-freq))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

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

