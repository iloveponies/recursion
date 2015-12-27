(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq)
      (first a-seq)
    (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
 (let [a (count seq-1)
        b (count seq-2)]
    (cond
      (< a b) seq-2
      (< b a) seq-1
      (< (apply max seq-1) (apply max seq-2)) seq-2
      :else seq-1)))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else
    (my-filter pred? (rest a-seq))
    ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (== elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
    []
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))
    )))

(defn power [n k]
(if (= k 0)
  1
  (* n (power n (dec k)))
  ))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))
    ))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))
    ))

(defn inits [a-seq]
 (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) [()]
    (singleton? a-seq) a-seq
    :else (cons (first a-seq)(rotations (rest a-seq)))
    ))

(defn my-frequencies-helper [freqs a-seq]
[:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
     []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
 (if (or (empty? coll) (= n 0))
     coll
     (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (count a-seq)
        a (int (/ n 2))
        b (- n a)]
   (vector (my-take a a-seq) (my-drop a a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) []
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons  (first a-seq) (seq-merge (vector (rest a-seq)) (vector(b-seq))))
    :else (rest a-seq)))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

