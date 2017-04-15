(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (next coll)
    (recur (next coll))
    (first coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)          false
    (== elem (first a-seq)) true
    :else                   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (or (empty? a-seq) (empty? b-seq))    (and (empty? a-seq) (empty? b-seq))
    (not (= (first a-seq) (first b-seq))) false
    :else                                 (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (take how-many-times (repeat what-to-repeat)))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs
        (first a-seq)
        (if (contains? freqs (first a-seq))
          (inc (get freqs (first a-seq)))
          1))
      (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (second (first a-map)) (first (first a-map)))
      (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? b-seq) a-seq
    (empty? a-seq) b-seq
    :else
      (if (<= (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (seq-merge
      (merge-sort (first (halve a-seq)))
      (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (apply concat
      (map
        (fn [x] (rotations (cons (first a-set) x)))
        (permutations (rest a-set))))))

(defn powerset [a-set]
  [:-])
