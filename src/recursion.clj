(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element-recur [a-seq, maxval]
  (if (empty? a-seq)
    maxval
    (max-element-recur (rest a-seq) (max (first a-seq) maxval))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max-element-recur a-seq (first a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2)) seq-1 seq-2))

(defn longest-sequence-recur [a-seq, lseq]
  (if (empty? a-seq)
    lseq
    (longest-sequence-recur (rest a-seq) (seq-max (first a-seq) lseq))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (longest-sequence-recur a-seq (first a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
          (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      (my-take-while (fn [x] nil) (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (cons (first a-seq) (my-drop-while (fn [x] nil) (rest a-seq))))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [a-seq, collected]
  (if (some (fn [x] (= a-seq x)) collected)
    collected
    (let [newseq (concat (rest a-seq) [(first a-seq)])]
      (rotations-helper newseq (cons a-seq collected)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq '())))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [k (first (first a-map))]
      (let [rv (repeat (get a-map k) k)]
        (concat rv (un-frequencies (dissoc a-map k)))))))

(defn my-take [n coll]
  (if (empty? coll)
    '()
    (if (> n 0)
      (cons (first coll) (my-take (dec n) (rest coll)))
      (my-take (dec n) '()))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (> n 0)
      (my-drop (dec n) (rest coll))
      (cons (first coll) (my-drop (dec n) (rest coll))))))


(defn halve [a-seq]
  (let [division (int (/ (count a-seq) 2))]
    [(my-take division a-seq) (my-drop division a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [parts (halve a-seq)]
      (seq-merge (merge-sort (first parts)) (merge-sort (second parts))))))

(defn split-into-monotonics-is-monotonic? [a-seq]
  (if (or (not (seq? a-seq)) (empty? a-seq))
    false
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [seqpart (first (filter split-into-monotonics-is-monotonic? (inits a-seq)))]
      (cons seqpart (split-into-monotonics (drop (count seqpart) a-seq))))))

(defn kokeilu? [a-seq]
  (do
    (println (str a-seq))
    true))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
