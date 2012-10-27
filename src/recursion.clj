(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (first coll) (empty? (rest coll)) false))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll)
    (my-last (rest coll))))

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
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

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
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map sort (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
  (map concat (butlast (tails a-seq)) (butlast (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)]
      (if (contains? freqs head)
        (my-frequencies-helper
          (assoc freqs head (inc (get freqs head)))
          (rest a-seq))
        (my-frequencies-helper
          (assoc freqs head 1)
          (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[key, value] (first a-map)]
      (concat (repeat value key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (zero? n) '()
    (empty? coll) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (zero? n) coll
    (empty? coll) '()
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [divisor (int (/ (count a-seq) 2))]
    [(my-take divisor a-seq) (my-drop divisor a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
            (cons
              (first a-seq)
              (seq-merge (rest a-seq) b-seq))
            (cons
              (first b-seq)
              (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[left, right] (halve a-seq)]
      (seq-merge
        (merge-sort left)
        (merge-sort right)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply < a-seq) (apply > a-seq))))

(defn split-into-monotonics [a-seq]
  (filter monotonic? (inits a-seq)))

(defn perm [so-far rest]
  (cond
    (empty? rest) so-far
    :else (map (fn [x] (perm (conj so-far x) (disj rest x))) rest)))

(defn permutations [a-set]
  (perm '() a-set))

(defn powerset [a-set]
  [:-])

