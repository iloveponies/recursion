(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max a-seq (longest-sequence (rest a-seq)))))

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
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [first (first a-seq)]
    (if (or (empty? a-seq) (not (pred? first)))
      '()
      (cons first (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (let [first (first a-seq)]
    (cond (empty? a-seq) '()
          (pred? first) (my-drop-while pred? (rest a-seq))
          :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)]
    (or (and a-empty b-empty)
        (and
          (not a-empty)
          (not b-empty)
          (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (let [a-empty (empty? seq-1)
        b-empty (empty? seq-2)]
    (if (or a-empty b-empty)
      '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (case k
    0 1
    1 n
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (< n 2) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (< how-many-times 1) '()
        (= how-many-times 1) [what-to-repeat]
        :else (cons what-to-repeat
                    (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    `()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (->> a-seq
      (reverse)
      (tails)
      (map reverse)
      (reverse)))

(defn rotations [a-seq]
  (if (seq a-seq)
    (my-map concat (drop-last (tails a-seq)) (inits a-seq))
    '(())))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (inc (get freqs k 0))]
      (my-frequencies-helper (assoc freqs k v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)
          x (repeat v k)]
      (concat x (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
    [(my-take x a-seq)
     (my-drop x a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (nil? a) b-seq
      (nil? b) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

