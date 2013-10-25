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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq) (or (max-element (rest a-seq)) 0))))

(defn seq-max [seq-1 seq-2]
  )

(defn longest-sequence [a-seq]
  [:-])

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
    a-seq
   (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
   :else
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq))  false
   (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (zero? n)
    0
    (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  )

(defn my-take [n coll]
  (seq (subvec coll 0 (min (count coll) n))))

(defn my-drop [n coll]
  (subvec coll (min (count coll) n)))

(defn halve [a-seq]
  [(subvec a-seq 0 (int (/ (count a-seq) 2))) (subvec a-seq (int (/ (count a-seq) 2)))])

(defn seq-merge-helper [new a-seq b-seq]
  (cond
   (empty? a-seq)
    (concat new b-seq)
   (empty? b-seq)
    (concat new a-seq)
   :else
    (if (<= (first a-seq) (first b-seq))
      (seq-merge-helper (conj new (first a-seq)) (rest a-seq) b-seq)
      (seq-merge-helper (conj new (first b-seq)) a-seq (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

