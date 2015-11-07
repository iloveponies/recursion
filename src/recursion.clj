(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))


(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq)(count b-seq)) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

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
  (if (<= n 1)
    n
    (+ (fib (dec n))
       (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat
                          (dec how-many-times)
                          what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (zero? (count a-seq))
    '(())
    (conj (tails (rest a-seq)) (seq a-seq))))

(defn inits [a-seq]
  (if (zero? (count a-seq))
    '(())
    (conj (inits (reverse (rest (reverse a-seq))))
          (seq a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq)
          (my-map (fn [a b] (concat a b))
                  (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (if (nil? (get freqs k))
              1
              (inc (get freqs k)))]
      (my-frequencies-helper (assoc freqs k v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [f (first a-map)]
      (concat (repeat (get f 1) (get f 0))
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (vector (my-take h a-seq) (my-drop h a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (let [f-a (first a-seq)
          pred (fn [n] (<= n (first a-seq)))]
      (seq-merge (rest a-seq)
                 (concat (take-while pred b-seq)
                         [(first a-seq)]
                         (drop-while pred b-seq))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

; rewrite it to not be so wasteful
(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono-inc (first
                    (drop-while (fn [b-seq] (apply (complement <) b-seq))
                                (inits a-seq)))
          mono-dec (first
                    (drop-while (fn [b-seq] (apply (complement >) b-seq))
                                (inits a-seq)))
          mono (if (< 1 (count mono-inc))
                 mono-inc
                 mono-dec)]
      (cons mono (split-into-monotonics (drop (count mono) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
