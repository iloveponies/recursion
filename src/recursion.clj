(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
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
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    []))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (- up-to 1) (my-range (- up-to 1)))
    []))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (my-map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [k (first a-seq)](if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs k (if (contains? freqs k) (+ (get freqs k) 1) 1))
      (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (concat (my-repeat (get (first a-map) 1) (get (first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (empty? coll) []
    (= 0 n) []
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) []
    (= 0 n) coll
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [f (int (/ (count a-seq) 2))
        s (- (count a-seq) f)]
    [(my-take f a-seq) (my-drop f a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) []
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[f s] (halve a-seq)]
      (seq-merge (merge-sort f) (merge-sort s)))))

(defn take-while-two-helper [pred? prev a-seq]
  (if (or (empty? a-seq) (not (pred? prev (first a-seq))))
    [prev]
    (cons prev (take-while-two-helper pred? (first a-seq) (rest a-seq)))))

(defn take-while-two [pred? a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (take-while-two-helper pred? (first a-seq) (rest a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [rising (take-while-two < a-seq)
        ascend (take-while-two > a-seq)
        longer (if (>= (count rising) 2) rising ascend)]
      (cons longer (split-into-monotonics (drop (count longer) a-seq))))))

(defn permutations-helper [rock-seq a-seq]
  (cond
    (empty? a-seq) [rock-seq]
    :else (apply concat (map (fn[x] (permutations-helper (cons (first a-seq) rock-seq) x)) (rotations (rest a-seq))))))

(defn permutations [a-set]
  (apply concat (map (fn[x] (permutations-helper [] x)) (rotations a-set))))

(defn powerset-helper [p-set a-set]
  (if (empty? a-set)
    p-set
    (apply concat (map (fn[x] (powerset-helper (conj p-set (set a-set)) x)) (rotations (rest a-set))))))

(defn powerset [a-set]
  (set (apply concat (map (fn[x] (powerset-helper #{#{}} x)) (rotations a-set)))))

