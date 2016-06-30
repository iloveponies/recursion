(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) []
    :else
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) []
    (empty? seq-2) []
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
    []))

(defn my-range [up-to]
  (cond
    (zero? up-to) []
    :else (cons (dec up-to)
                (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (map (fn [i] (concat (drop i a-seq) (take i a-seq) )) (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else
    (let [
           f (first a-seq)
           b (boolean (freqs f))
           v (if b
               (+ (freqs f) 1)
               1)
           ]
      (my-frequencies-helper (assoc freqs f v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    [] ;empty sequence
    (let [a-key (key (first a-map))
          a-val (val (first a-map))]
      (concat (my-repeat a-val a-key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (zero? n) (empty coll)
    :else (cons (first coll)
                (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (== n 0) coll
    (empty? coll) coll
    :else (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [deal (int (/ (count a-seq) 2))]
    [(my-take deal a-seq) (my-drop deal a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (let [
           f (first a-seq)
           test? (fn [x] (<= x f))
           pref (my-take-while test? b-seq)
           suff (my-drop-while test? b-seq)
           ]
      (concat
        pref
        (cons f '())
        (seq-merge (rest a-seq) suff)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[half-seq1 half-seq2] (halve a-seq)]
      (seq-merge (merge-sort half-seq1) (merge-sort half-seq2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

