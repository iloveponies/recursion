(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn empty-or-singleton? [coll]
  (or
    (empty? coll)
    (singleton? coll)))

(defn my-last [coll]
  (if (empty-or-singleton? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty-or-singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (last (sort-by count [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (if (empty-or-singleton? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [first-element (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? first-element)
        (cons first-element (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (= elem (first a-seq))
      true
    (empty? a-seq)
      false
    :else
      (sequence-contains? elem (rest a-seq))))

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
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq)
         (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (let [a-rest (rest a-seq)
            b-rest (rest b-seq)]
        (if (= (not (empty? a-rest)) (not (empty? b-rest)))
          (seq= a-rest b-rest)
          false))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
      (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (vector [])
    (conj (inits (drop-last a-seq)) a-seq)))

(defn rotate [a-seq n]
  (if (zero? n)
    '()
    (cons a-seq (rotate (concat (rest a-seq) [(first a-seq)]) (dec n)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '[[]]
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)
          new-freqs (assoc freqs first-element
                      (inc (or (get freqs first-element) 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[key, freq]] (repeat freq key)) a-map)))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-point (int (/ (count a-seq) 2))]
    (vector (my-take split-point a-seq) (my-drop split-point a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a-empty  (empty? a-seq)
        b-empty  (empty? b-seq)
        a-first  (first a-seq)
        b-first  (first b-seq)]
    (cond
      (and (not a-empty) (not b-empty))
        (if (<= a-first b-first)
          (cons a-first (seq-merge (rest a-seq) b-seq))
          (cons b-first (seq-merge a-seq (rest b-seq))))
      (and a-empty (not b-empty))
        (concat [b-first] (rest b-seq))
      (and (not a-empty) b-empty)
        (concat [a-first] (rest a-seq))
      :else
        '())))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn is-monotonic? [a-seq]
  (if (< (count a-seq) 2)
    true
    (or
      (apply < a-seq)
      (apply > a-seq))))

(defn split-into-monotonics-helper [a-seq n]
  (let [new-seq (drop n a-seq)]
    (if (empty? new-seq)
      '[]
      (let [monotonic-seq (take-while is-monotonic? (inits new-seq))]
        (cons (last monotonic-seq)
              (split-into-monotonics-helper new-seq (dec (count monotonic-seq))))))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper a-seq 0))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

