(ns recursion)

(defn product [coll]
  (if (empty? coll)
   1
   (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (if (not (empty? coll))
    (empty? (rest coll))
    false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (if (< (first a-seq) (first (rest a-seq)))
        (max-element (rest a-seq))
        (max-element (rest (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (if (< (count (first a-seq)) (count (first (rest a-seq))))
        (longest-sequence (rest a-seq))
        (longest-sequence (subvec a-seq 2))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (if (and (not (empty? a-seq)) (pred? (first a-seq)))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (= (count a-seq) (count b-seq)) (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq))
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
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (butlast a-seq)))))

(defn rotations-helper [a-seq n]
  (if (zero? n)
    []
    (let [rotated (concat [(last a-seq)] (butlast a-seq))]
      (cons rotated (rotations-helper rotated (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item      (first a-seq)
          new-freqs (if (contains? freqs item)
                      (assoc freqs item (inc (get freqs item)))
                      (assoc freqs item 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (into {} (reverse (my-frequencies-helper {} a-seq))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [pair (first a-map)]
      (concat (repeat (last pair) (first pair)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
   (let [halfway (int (/ (count a-seq) 2))]
    (cons (my-take halfway a-seq) [(my-drop halfway a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :default (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [half1 (first (halve a-seq))
          half2 (last (halve a-seq))]
      (seq-merge (merge-sort half1) (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
