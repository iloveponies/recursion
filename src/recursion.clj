(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
     (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
    ))

(defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2
    ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))
    ))

(defn sequence-contains? [elem a-seq]
  (if (= elem (first a-seq))
    true
    (if (empty?(rest a-seq))
      false
      (sequence-contains? elem (rest a-seq))
    )))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      []
    )))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (not (pred? (first a-seq)))
      a-seq
      (my-drop-while pred? (rest a-seq))
    )))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (and true (seq= (rest a-seq) (rest b-seq)))
   :else false
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (not (or (empty? seq-1) (empty? seq-2))) (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   :else []
   ))


(defn power [n k]
  (cond
   (= 0 n k) nil
   (= 0 n) 0
   (= 0 k) 1
   :else (* n (power n (- k 1)))
   ))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   (= 2 n) 1
   (< 2 n) (+ (fib (- n 1)) (fib (- n 2)))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat ))
    ()
    ))

(defn my-range [up-to]
 (cond
  (= 0 up-to) ()
  (= 1 up-to) (seq [0] )
  :else (cons (+ 1 (first (my-range (dec up-to))) )  (my-range (dec up-to)))
  ))

(defn tails [a-seq]
 (if (empty? a-seq)
   (cons () ())
   (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
 [:-])

(defn my-frequencies [a-seq]
 [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (cond
   (empty? coll) ()
   (< 1 n ) (cons (first coll) (my-take (dec n) (rest coll)))
   (= 1 n ) (cons (first coll) ())
    ))

(defn my-drop [n coll]
  (if (< 0 n)
    (my-drop (dec n) (rest coll))
    coll
    ))

(defn halve [a-seq]
  (if (= 1 (count a-seq))
    (vector () a-seq)
    (vector (my-take (int (/ (count a-seq) 2)) a-seq ) (my-drop (int (/ (count a-seq) 2)) a-seq )))
  )

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   ))

(defn merge-sort [a-seq]
  (let [[l r] (halve a-seq)]
    (if (> 2 (count a-seq))
     a-seq
    (seq-merge (merge-sort l) (merge-sort r))
    )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

