(ns recursion)

(defn product [coll]
  (if
    (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (not (empty? coll)) (empty? (rest coll)) false))

(defn my-last [coll]
  (if (not (empty? (rest coll))) (my-last (rest coll)) (first coll)))

(defn max-element [a-seq]
  (if
    (empty? a-seq)
    nil
    (max (first a-seq)
         (if (empty? (rest a-seq))
           (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if
    (empty? a-seq)
    []
    (if
      (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   (not (pred? (first a-seq))) a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if
    (empty? a-seq)
    [()]
    (cons
     a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if
    (empty? a-seq)
    [()]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))
(defn rotations-helper [n a-seq]
  (if
    (== n (count a-seq))
    ()
    (cons
     (concat a-seq ())
     (rotations-helper
      (inc n)
      (concat (rest a-seq) [(first a-seq)])))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (rotations-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if
    (empty? a-seq)
    freqs
    (my-frequencies-helper
     (assoc
       freqs
       (first a-seq)
       (if
         (contains? freqs (first a-seq))
         (inc
          (get freqs (first a-seq)))
         1))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if
   (empty? a-map) ()
   (let [[a b] (first a-map)]
     (concat
      (repeat b a)
      (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n)) () (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if
    (or (zero? n) (empty? coll)) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if
    (empty? a-seq)
    ()
    (let
      [a (int (/ (count a-seq) 2))]
      [(my-take a a-seq) (my-drop a a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if
           (< (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let
      [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) ()
    (let [c (count a-seq)]
      (if
       (== (mod c 2) 0)
        (cons [(first a-seq) (second a-seq)] (split-into-monotonics (rest (rest a-seq))))
        (let [[a b c] a-seq] (cons [a b c] (split-into-monotonics (rest (rest (rest a-seq))))))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

