(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
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
  (if (or (singleton? a-seq) (empty? a-seq))
      (first a-seq)
      (longest-sequence (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
       (first a-seq)
       (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (= elem (first a-seq))
     true
   (empty? a-seq)
     false
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons
      (first a-seq)
      (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or
    (or (empty? a-seq) (empty? b-seq))
    (not (= (first a-seq) (first b-seq)))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons
     (dec up-to)
     (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons
     (seq a-seq)
     (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons
     (seq a-seq)
     (inits (drop-last a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)]
      (my-frequencies-helper
       (update-in freqs [k] (fnil inc 0))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat
     (repeat (val (first a-map)) (key (first a-map)))
     (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons
     (first coll)
     (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (not (zero? n))
      (my-drop (dec n) (rest coll))
      (cons
       (first coll)
       (my-drop n (rest coll))))))

(defn halve [a-seq]
  (let [k (int (/ (count a-seq) 2))]
    (vector
     (my-take k a-seq)
     (my-drop k a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
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
    (if (< (count a-seq) 2)
      a-seq
      (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [s] (if (empty? s)
                             true
                             (or (apply <= s) (apply >= s))))
        monotonic-prefixes (fn [s] (take-while monotonic? (reverse (inits s))))
        l (fn [s] (apply max (map count (monotonic-prefixes a-seq))))
        k (l a-seq)]
    (if (empty? a-seq)
      '()
       (cons
        (take k a-seq)
        (split-into-monotonics (drop k a-seq))))))

(defn permutations [a-set]
  (let [permutations-by-level
        (fn [rotation]
          (map (fn [permutation]
                 (cons
                  (first rotation)
                  permutation))
               (permutations (rest rotation))))]
    (cond
     (empty? a-set) '(())
     (== (count a-set) 1) (list a-set)
     :else (apply concat (map permutations-by-level (rotations a-set))))))


(defn powerset [a-set]
  (if (empty? a-set)
    '#{#{}}
    (cons
     (set a-set)
     (set (apply concat (map powerset (map rest (rotations a-set))))))))

