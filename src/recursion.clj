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
  (cond
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (every? empty? [a-seq b-seq]) true
    (some empty? [a-seq b-seq]) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (some empty? [seq-1 seq-2])
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
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
  (if (empty? a-seq)
    '(())
    (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem      (first a-seq)
          freq      (if (contains? freqs elem) (freqs elem) 0)
          new-freqs (assoc freqs elem (inc freq))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[elem freq]] (repeat freq elem)) a-map)))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (/ (count a-seq) 2)
        fst-half (my-take half a-seq)
        snd-half (my-drop half a-seq)]
    [fst-half snd-half]))

(defn seq-merge [a-seq b-seq]
  (cond
    (every? empty? [a-seq b-seq]) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (let [[lo hi] (if (< (first a-seq) (first b-seq)) [a-seq b-seq] [b-seq a-seq])]
        (cons (first lo) (seq-merge hi (rest lo))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a-monotonic (first (drop-while (complement monotonic?) (inits a-seq)))
          remainder   (drop (count a-monotonic) a-seq)]
      (cons a-monotonic (split-into-monotonics remainder)))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (list (sequence a-set))
    (let [step (fn [a-seq]
                 (map (partial cons (first a-seq)) (permutations (rest a-seq))))]
      (apply concat (map step (rotations a-set))))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    (not (set? a-set)) (powerset (set a-set))
    :else
      (let [subsets (set (map (partial disj a-set) a-set))]
        (conj (set (apply concat (map powerset subsets))) a-set))))
