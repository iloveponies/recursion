(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or
        (singleton? coll)
        (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or
        (empty? a-seq)
        (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or
        (empty? a-seq)
        (singleton? a-seq))
    (first a-seq)
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
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
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
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* (power n (- k 1)) n)))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (inits (drop-last a-seq)))))

(defn rotations [a-seq]
  (let [rot (fn rot [a-seq n]
              (if (< n 1)
                '()
                (cons a-seq (rot
                              (conj (vec (rest a-seq)) (first a-seq))
                              (dec n)))))]
    (if (empty? a-seq)
      '(())
      (rot a-seq (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-seq-first (first a-seq)]
      (my-frequencies-helper
        (assoc freqs a-seq-first (if (contains? freqs a-seq-first)
                             (inc (get freqs a-seq-first))
                             1))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map a-seq]
  (if (empty? a-map)
    a-seq
    (let [a-key (first a-map)]
      (un-frequencies-helper (rest a-map) (concat (repeat (second a-key) (first a-key)) a-seq)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map '()))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [halving-point (int (/ (count a-seq) 2))]
    (vector (my-take halving-point a-seq) (my-drop halving-point a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (< (count a-seq) 4)
    a-seq
    (let [s-last (take-last 2 a-seq)
          s-first (split-into-monotonics (drop-last 2 a-seq))]
      (if (seq? (first s-first))
        (concat s-first [s-last])
        (seq [s-first s-last])))))

(defn drop-at [i a-set]
  (vec (concat
         (take i a-set)
         (drop (inc i) a-set))))

(defn permutations [a-set]
  (let [helper (fn helper [perm source]
                 (if (== 0 (count source))
                   [perm]
                   (loop [n 0
                          result []]
                     (if (== n (count source))
                       result
                       (recur
                         (inc n)
                         (concat result (helper
                                          (conj perm (get source n))
                                          (drop-at n source))))))))]
  (helper [] a-set)))

(defn powerset [a-set]
  (if (empty? a-set)
    [[]]
    (loop [acc [[]]
           s-first (first a-set)
           s-rest (rest a-set)]
      (let [result (concat (map (fn [x] (conj x s-first)) acc) acc)]
        (if (empty? s-rest)
          result
          (recur
            result
            (first s-rest)
            (rest s-rest)))))))
