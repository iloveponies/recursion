(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

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
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
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
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0)
      0
    (== n 1)
      1
    :else
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (sequence a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reversed-inits (tails (reverse a-seq))]
    (reverse (map reverse reversed-inits))))

(defn rotations [a-seq]
  (let [ts (tails a-seq)
        is (inits a-seq)
        rots (my-map concat (rest ts) (rest is))]
    (if (empty? rots)
      '(())
      rots)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-count (if (contains? freqs elem)
                       (inc (get freqs elem))
                       1)]
      (my-frequencies-helper (assoc freqs elem elem-count)
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (my-repeat (second (first a-map))
                       (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll)
            (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (<= n 0)
      coll
    (empty? coll)
      '()
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq) (my-drop mid a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq)
            (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq)
            (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves))
                 (merge-sort (second halves))))))

(defn longest-mono-helper [pred? prev a-seq]
  (cond
    (empty? a-seq)
      (list prev)
    (pred? prev (first a-seq))
      (concat (list prev) (longest-mono-helper pred? (first a-seq) (rest a-seq)))
    :else
      (list prev)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [growing (longest-mono-helper (fn [x y] (< x y))
                                       (first a-seq)
                                       (rest a-seq))
          decreasing (longest-mono-helper (fn [x y] (> x y))
                                          (first a-seq)
                                          (rest a-seq))
          growing-count (count growing)
          decreasing-count (count decreasing)]
      (if (> growing-count 1)
        (cons growing (split-into-monotonics (drop growing-count a-seq)))
        (cons decreasing (split-into-monotonics (drop decreasing-count a-seq)))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat
           (map (fn [x] (map (fn [y] (cons (first x) y)) (permutations (rest x))))
                (rotations a-set)))))

(defn powerset [a-set]
  [:-])

