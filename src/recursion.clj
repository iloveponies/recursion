(ns recursion)

(defn product [coll]
   (if (empty? coll) 1
     (* (first coll) (product (rest coll)))
   )
)

(defn singleton? [coll]
   (if (empty? coll) false
     (empty? (rest coll))
   )
)

(defn my-last [coll]
  (cond
     (empty? coll) nil
     (singleton? coll) (first coll)
     :else (my-last (rest coll))
  )
)

(defn max-element [a-seq]
    (cond
       (empty? a-seq) nil
       (singleton? a-seq) (first a-seq)
       :else (max (first a-seq) (max-element (rest a-seq)))
    )
)


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
  )
)

(defn longest-sequence [a-seq]
   (if (empty? a-seq) nil
     (seq-max (first a-seq) (longest-sequence (rest a-seq)))
   )
)

(defn my-filter [pred? a-seq]
   (cond
      (empty? a-seq)        a-seq
      (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      :else                 (my-filter pred? (rest a-seq))
   )
)

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))
  )
)

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                 '()
  )
)


(defn my-drop-while [pred? a-seq]
  (cond
     (empty? a-seq) a-seq
     (not (pred? (first a-seq))) (seq a-seq)
     (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   )
)

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (not= (count a-seq) (count b-seq)) false
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false
  )
)


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
  )
)



(defn power [n k]
  (if (zero? k)
    1
    (* (power n (dec k)) n)
  )
)


(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (- n 2)) (fib (dec n)))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  )
)

(defn my-range [up-to]
  (if (zero? up-to) '()
      (cons (dec up-to) (my-range (dec up-to)))
  )
)


(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))
  )
)


(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
)

(defn rotations [a-seq]
  [:-])



(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

