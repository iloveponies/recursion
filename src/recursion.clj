(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))
    )
  )
)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false
    )
  )
)

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))
  )
)

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
  )
)

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
  )
)

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (if (> (count (first a-seq)) (count (longest-sequence (rest a-seq))))
      (first a-seq)
      (longest-sequence (rest a-seq))
    )
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq))
    )
  )
)

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '()
    )
  )
)

(defn my-drop-while [pred? a-seq]
  (if (empty? (rest a-seq))
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (cons (first a-seq) (rest a-seq))
    )
  )
)

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false
    )
  )
)

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
  )
)

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k))))
)

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  )
)

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))
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
