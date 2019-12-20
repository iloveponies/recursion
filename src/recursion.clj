(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (= (rest coll) '()) (not (empty? coll))))

(defn my-last [coll]
  (cond
    (empty? coll)
      nil
    (singleton? coll)
      (first coll)
    :else
      (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (if (> (first a-seq) (first (rest a-seq)))
        (max-element (cons (first a-seq) (rest (rest a-seq))))
        (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (longest-sequence (cons (seq-max (first a-seq) (first (rest a-seq))) (rest (rest a-seq))))))

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
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      () )))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (not (= (count a-seq) (count b-seq)))
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
    (= n 1)
      1
    (= n 0)
      0
    :else
      (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (conj '() a-seq)
    (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '( () )
  (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)]
      (my-frequencies-helper
        (if (contains? freqs x)
          (assoc freqs x (inc (get freqs x)))
          (assoc freqs x 1))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (my-repeat (last (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    ()
  (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      ()
    (<= n 0)
      (seq coll)
  :else
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    ()
  (let [x (int (/ (count a-seq) 2))]
    (conj [] (my-take x a-seq) (my-drop x a-seq)))))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    ()
    (let [a (cond
              (empty? a-seq)
                true
              (empty? b-seq)
                false
              (> (first a-seq) (first b-seq))
                true
              :else
                false)]
      (conj (if a
              (seq-merge (rest b-seq) a-seq)
              (seq-merge (rest a-seq) b-seq))
            (if a (first b-seq) (first a-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

