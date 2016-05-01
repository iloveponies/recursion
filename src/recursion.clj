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
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (< (count a-seq) 2)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (not (= (first a-seq) elem))
      (sequence-contains? elem (rest a-seq))
    :else
      true
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (and (empty? a-seq) (not (empty? b-seq)))
      false
    (and (not (empty? a-seq)) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
       false))

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
  (cond
    (zero? n)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1))
         (fib (- n 2)))))

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
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper
        (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
        (rest a-seq))
      (my-frequencies-helper
        (assoc freqs (first a-seq) 1)
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[key value] (first a-map)]
      (concat (repeat value key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [divider (int (/ (count a-seq) 2))]
  (vector (my-take divider a-seq) (my-drop divider a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (and (empty? a-seq) (empty? b-seq))
      '()
    :else
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (<= (count a-seq) 2)
    [a-seq]
    (let [sub-monotonic (take-while monotonic? (rest (inits a-seq)))]
      (cons (last sub-monotonic) (split-into-monotonics (drop (count sub-monotonic) a-seq))))))

(defn permutations [a-set]
  (cond
    (empty? a-set)
      (list '())
    (= 1 (count a-set))
      (list a-set)
    :else
      (for [head a-set
            tail (permutations (disj (set a-set) head))]
        (do
          (cons head tail)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (clojure.set/union (powerset (next a-set))
        (map #(conj % (first a-set)) (powerset (next a-set))))))

