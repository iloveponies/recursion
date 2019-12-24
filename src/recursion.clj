(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (my-last (rest a-seq))))


(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    ((complement =) elem (first a-seq)) (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          ((complement pred?) (first a-seq)))
    '()
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

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
  (cond
    (zero? n) n
    (== 1 n) n
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
    (if (< n 0)
      '()
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq)
                      (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-e (first a-seq)
          new-freqs (if (contains? freqs first-e)
                      (assoc freqs first-e (inc (get freqs first-e)))
                      (assoc freqs first-e 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[e n]] (repeat n e)) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll)
          (< n 1))
    '()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    (vector (my-take div a-seq)
            (my-drop div a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

