(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
  (if (> (count seq-1)(count seq-2))
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
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (== (count a-seq) (count b-seq) )) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq) )
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (and (not (empty? seq-1)) (not (empty? seq-2)))
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))
   :else '()))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
    0
   (== n 1 )
    1
   :else
    (+ (fib (- n 2)) (fib (dec n)) )))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq)) )))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn inits2 [a-seq]
  (reverse (inits a-seq)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
     (empty? a-seq)
      freqs
     (contains? freqs f)
      (my-frequencies-helper
        (assoc freqs f (inc (get freqs f)) ) r)
     :else
      (my-frequencies-helper
        (assoc freqs f 1) r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map a-seq]
    (let [f (first a-map)
          r (rest a-map)]
      (if (empty? a-map)
        a-seq
        (un-frequencies-helper r
                               (concat a-seq (repeat (val f) (key f)) )))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map '() ))

(defn my-take-helper [n coll a-seq]
  (if (or (zero? n) (empty? coll))
    a-seq
    (my-take-helper (dec n) (rest coll) (concat a-seq [(first coll)]) )))

(defn my-take [n coll]
  (my-take-helper n coll '()))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (cons (my-take n a-seq) (cons (my-drop n a-seq) []))))

(defn seq-merge-helper [res-seq a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      res-seq
    (and (not (empty? a-seq)) (not (empty? b-seq)))
      (if (>= a b)
        (seq-merge-helper (concat res-seq [b]) a-seq (rest b-seq))
        (seq-merge-helper (concat res-seq [a]) (rest a-seq) b-seq))
    (and (not (empty? a-seq)) (empty? b-seq))
      (seq-merge-helper (concat res-seq [a]) (rest a-seq) b-seq)
    (and (empty? a-seq) (not (empty? b-seq)))
      (seq-merge-helper (concat res-seq [b]) a-seq (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (let [left  (nth (halve a-seq) 0)
        right (nth (halve a-seq) 1)]
    (if (>= 1 (count a-seq))
      a-seq
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

