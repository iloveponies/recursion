(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
        (first coll)
        (my-last (rest coll)))))

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
    (= elem (first a-seq))
      true
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
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? n)
    0
    (if (zero? k)
      1
      (* n (power n (dec k))))))

(defn fib [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (cond
    (= up-to 0)
      '()
    (= up-to 1)
      '(0)
    (> up-to 0)
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (cons ()())
    :else
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq)
      (cons () ())
    :else
      (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq)
      freqs
    :else
      (let [first-elem (first a-seq)
            elem-count (if (freqs first-elem)
                            (+ (freqs first-elem) 1) 1)]
            (my-frequencies-helper (assoc freqs first-elem elem-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map)
      a-map
    :else
      (let [[elem num] (first a-map)]
        (concat (repeat num elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll)
      '()
    (< n 1)
      '()
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      coll
    (< n 1)
      coll
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [num (count a-seq)
        jako (int (/ num 2))]
        (vector (my-take jako a-seq) (my-drop jako a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    :else
      (let [first-a (first a-seq)
            first-b (first b-seq)]
            (if (< first-a first-b)
              (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
              (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1)
      a-seq
    :else
      (let [[first-half second-half] (halve a-seq)]
        (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
