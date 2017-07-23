(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
   (if (and (not (empty? coll))
           (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

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
    (not (= elem (first a-seq)))
      (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (my-drop-while pred? (drop 1 a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (== 1 n) 1
    (== 2 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< 0 how-many-times)
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    :else ()))

(defn my-range [up-to]
  (cond
    (< 0 up-to)
      (cons (dec up-to) (my-range (dec up-to)))
    :else ()))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (cons a-seq ())
    :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond (empty? a-seq) (concat [] [()])
    :else (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (not (contains? freqs (first a-seq)))
      (let [new-freqs (assoc freqs (first a-seq) 1)]
        (my-frequencies-helper new-freqs (rest a-seq)))
      (my-frequencies-helper
        (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (if (empty? a-map)
    result
    (let [[key value] (first a-map)]
      (let [new-result (concat result (repeat value key))]
        (un-frequencies-helper new-result (rest a-map))))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
    (cond
    (not (empty? coll))
      (if (not (zero? n))
            (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      coll
    (zero? n)
      coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (if (== 0 mid)
      (vector () (cons (first a-seq) ()))
      (vector (my-take mid a-seq) (my-drop mid a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      a-seq
    (or (empty? a-seq) (empty? b-seq))
      (if (empty? a-seq)
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
    (<= (first a-seq) (first b-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (<= (first b-seq) (first a-seq))
       (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (>= 1 (count a-seq))
      (into () a-seq)
    :else (let [[first-halve rest-halve] (halve a-seq)]
            (seq-merge
              (merge-sort first-halve)
              (merge-sort rest-halve)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

