(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)]
      (if (pred? x)
        (cons x (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (< 0 k)
    (* n (power n (- k 1)))
    1))

(defn fib [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    []))

(defn my-range [up-to]
  (if (< 0 up-to)
    (cons (- up-to 1) (my-range (- up-to 1)))
    []))

(defn tails [a-seq]
  (if (empty? a-seq)
    [ () ]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [()]
    (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [v (first a-seq)
          f (get freqs v)]
      (my-frequencies-helper
        (if f
          (assoc freqs v (+ f 1))
          (assoc freqs v 1))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[v n]] (repeat n v)) a-map)))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    []
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (vector (my-take n a-seq) (my-drop n a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[xs ys] (halve a-seq)]
      (seq-merge (merge-sort xs) (merge-sort ys)))))
    

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [ismonotonic (fn [x] (or (or (empty? x) (apply >= x)) (apply <= x)))
          xs (last (take-while ismonotonic (reverse (inits a-seq))))]
      (cons xs (split-into-monotonics (drop (count xs) a-seq))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) [()]
    (singleton? a-set) [a-set]
    :else
      (let [concx (fn [ys]
                    (map (fn [i] (concat (take i ys) [(first a-set)] (drop i ys)))
                         (range 0 (+ 1 (count ys)))))
            xs (rest a-set)]
        (apply concat (map concx (permutations (rest a-set)))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [x (first a-set)
          xs (rest a-set)
          xss (powerset xs)]
      (concat xss (map (fn [ys] (conj ys x)) xss)))))


