(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq)
      (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq)
      (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (cons (first a-seq)
      (my-filter pred? (rest a-seq)))
    :else
    (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (cons (first a-seq)
      (my-take-while pred? (rest a-seq)))
    :else
    ()))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (or (and (not (empty? a-seq)) (empty? b-seq))
      (and (empty? a-seq) (not (empty? b-seq))))
    false
    (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
    :else
    false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0)
    0
    (= n 1)
    1
    (>= n 2)
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (my-map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (= (get freqs (first a-seq)) nil)
      (my-frequencies-helper
        (assoc freqs (first a-seq) 1)
        (rest a-seq))
      (my-frequencies-helper
        (assoc freqs
          (first a-seq)
          (inc (get freqs (first a-seq))))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [first-key (first (first a-map))]
      (concat
        (take (a-map first-key) (repeat first-key))
        (un-frequencies (dissoc a-map first-key))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (conj
    (vector (my-take (int (/ (count a-seq) 2)) a-seq))
    (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
    b-seq
    (empty? b-seq)
    a-seq
    (<= (first a-seq) (first b-seq))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1)
    a-seq
    :else
    (seq-merge (merge-sort (first (halve a-seq)))
      (merge-sort (second (halve a-seq))))))

(defn monotonically-increasing-counter [counter number a-seq]
  (cond
    (empty? a-seq)
    counter
    (singleton? a-seq)
    (if (> (first a-seq) number)
      (inc counter)
      counter)
    :else
    (if (> (first a-seq) number)
      (monotonically-increasing-counter
        (inc counter)
        (first a-seq)
        (rest a-seq))
      counter)))

(defn monotonically-decreasing-counter [counter number a-seq]
  (cond
    (empty? a-seq)
    counter
    (singleton? a-seq)
    (if (< (first a-seq) number)
      (inc counter)
      counter)
    :else
    (if (< (first a-seq) number)
      (monotonically-decreasing-counter
        (inc counter)
        (first a-seq)
        (rest a-seq))
      counter)))

(defn number-of-monotonic-elems [a-seq]
  (let
    [a (monotonically-increasing-counter
         1
         (first a-seq)
         (rest a-seq))
     b (monotonically-decreasing-counter
         1
         (first a-seq)
         (rest a-seq))]
    (if (> a b) a b)))


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [mono-seq
          (take (number-of-monotonic-elems a-seq) a-seq)]
      (cons
        mono-seq
        (split-into-monotonics (drop (count mono-seq) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
