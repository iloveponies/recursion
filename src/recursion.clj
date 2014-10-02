(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

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
    (not (= elem (first a-seq)))
      (sequence-contains? elem (rest a-seq))
    :else
      true))

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
    (or (empty? a-seq) (empty? b-seq))
      (if (and (empty? a-seq) (empty? b-seq)) true false)
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
 (if (empty? a-seq)
   '(())
   (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

; XXX: this is definitely not the correct solution but it works...
(defn rotations [a-seq]
  (let [seq-inits (reverse (inits a-seq))]
    (let [seq-tails (tails a-seq)]
      (if (empty? a-seq)
        '(())
        (map concat (rest seq-tails) (rest seq-inits))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)]
      (let [v (if (contains? freqs k) (inc (get freqs k)) 1)]
        (my-frequencies-helper (assoc freqs k v) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [n k a-seq]
  (if (= n 0)
    a-seq
    (cons k (un-frequencies-helper (dec n) k a-seq))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [m (first a-map)]
      (let [k (first m)]
        (let [n (second m)]
          (concat (un-frequencies (rest a-map)) (un-frequencies-helper n k '())))))))

(defn my-take-helper [n pos new-coll coll]
  (if (or (= n pos) (empty? coll))
    new-coll
    (cons (first coll) (my-take-helper n (inc pos) new-coll (rest coll)))))

(defn my-take [n coll]
  (my-take-helper n 0 '() coll))

(defn my-drop-helper [n pos coll]
  (if (or (= n pos) (empty? coll))
    coll
    (my-drop-helper n (inc pos) (rest coll))))

(defn my-drop [n coll]
  (my-drop-helper n 0 coll))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (vector (my-take n a-seq) (my-drop n a-seq))))

; XXX: inside of cond could cleary be simplified...
(defn seq-merge-helper [a-seq b-seq m-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    m-seq
    (cond
      (empty? a-seq)
        (cons (first b-seq) (seq-merge-helper a-seq (rest b-seq) m-seq))
      (empty? b-seq)
        (cons (first a-seq) (seq-merge-helper (rest a-seq) b-seq m-seq))
      (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge-helper (rest a-seq) b-seq m-seq))
      :else
        (cons (first b-seq) (seq-merge-helper a-seq (rest b-seq) m-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq '()))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [half (halve a-seq)]
      (seq-merge (merge-sort (first half)) (merge-sort (second half))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

