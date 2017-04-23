(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn one-or-less? [coll]
  (or (empty? coll) (empty? (rest coll))))

(defn my-last [coll]
  (nth coll (dec (count coll)) nil))

(defn max-element [a-seq]
  (if (one-or-less? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (one-or-less? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (and
       (not (empty? a-seq))
       (pred? (first a-seq)))
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))
    '()))

(defn my-drop-while [pred? a-seq]
  (if (and
       (not (empty? a-seq))
       (pred? (first a-seq)))
    (my-drop-while pred? (rest a-seq))
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    ;; (= (first [nil]) (first [])) is of course true...
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))


(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    '()
    (cons
     (f (first a-seq) (first b-seq))
     (my-map f (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector '())
    (concat (vector a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  ;; Not quite sure about this..
  (map reverse (tails (reverse a-seq))))

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

