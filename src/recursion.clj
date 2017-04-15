(ns recursion)

(defn product [coll]
  (cond 
    (empty? coll) 1
    :else (* (first coll) (product (rest coll)) )
    ))



(defn singleton? [coll]
  (if (empty? coll)
      false
      (empty? (rest coll))
    )
)

(defn my-last [coll]
  (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))
    )
  )


(defn max-element-helper [v a-seq]
  (if (empty? (rest a-seq))
      (max v (first a-seq))
      (max-element-helper (max v (first a-seq)) (rest a-seq))
  )
)

(defn max-element [a-seq]
  (if (empty? a-seq)
      nil
      (max-element-helper (- 1 (first a-seq)) a-seq)    
  )
)

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2
    ))


(defn max-seq-helper [v a-seq]
  (if (empty? (rest a-seq))
      (seq-max v (first a-seq))
      (max-seq-helper (seq-max v (first a-seq)) (rest a-seq))
  )
)

(defn longest-sequence [a-seq]
  (max-seq-helper [] a-seq)
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))
  ))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
  )
)


(defn my-take-while [pred? a-seq]
  (cond 
      (empty? a-seq) '()
      (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      :else '()
  )
)

(defn my-drop-while [pred? a-seq]
  (cond 
      (empty? a-seq) '()
      (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
      :else a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
  )
)

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
  )
)

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))
    ))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    )
)

(defn my-range [up-to]
  (
    if (== 0 up-to)
      '()
      (conj (my-range (dec up-to)) (dec up-to))
    ))

(defn head [a-seq]
  (if (empty? (rest a-seq))
      '()
      (cons (first a-seq) (head (rest a-seq)))
    )
)

(defn tail [a-seq]
  (reverse (head (reverse a-seq)))
  )

(defn tails-helper [a-seq]
  (if (empty? a-seq)
      '()
      (cons (map (fn [x] x) a-seq) (tails-helper (rest a-seq)))
      )
  )

(defn tails [a-seq]
  (reverse (cons '() (reverse (tails-helper a-seq))))
  )

(defn inits [a-seq]
    (map reverse (reverse (tails (reverse a-seq))))
  )

(defn rotate [a-seq]
  (concat (rest a-seq) [(first a-seq)])
  )


(defn rotations-helper [a-seq]
  (if (empty? a-seq)
      nil
      (cons (first (sequence a-seq)) (rotations-helper (map rotate (rest a-seq))))
    )
  )

(defn rotations [a-seq]
  (if (empty? a-seq)
    (sequence ['()])
    (rotations-helper (my-repeat (count a-seq) a-seq))
    )
  )



(defn update-or-increment [freqs k]
  (if (contains? freqs k)
      (assoc freqs k (inc (get freqs k)))
      (assoc freqs k 1)
  )
)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? (rest a-seq))
      (update-or-increment freqs (first a-seq))
      (my-frequencies-helper (update-or-increment freqs (first a-seq)) (rest a-seq))
  ))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
      {}
      (my-frequencies-helper {} a-seq )
  ))

(defn un-frequencies [a-map]
  (apply concat (map (fn [v] (repeat (get a-map v) v )) (keys a-map)))
  )

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
      '()
      (cons (first coll) (my-take (dec n) (rest coll)))
    ))

(defn my-drop [n coll]
  (if (== n 0)
      coll
      (my-drop (dec n) (rest coll))
  ))

(defn halve [a-seq](
  let [h (int (/ (count a-seq) 2))]
  [ (my-take h a-seq) (my-drop h a-seq) ]
  )
)

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
  )
)

(defn merge-sort [a-seq](
  let [[h1 h2] (halve a-seq)]
  (cond
    (empty? a-seq) a-seq
    (== (count a-seq) 1) a-seq
    :else (seq-merge (merge-sort h1) (merge-sort h2))
)))



(defn split-into-monotonics [a-seq]
  ( )
)


(defn permutations [a-set]
  (cond
    (empty? a-set) (list '())
    (= 1 (count a-set)) (list a-set)
    :else (for [elem a-set others (permutations (disj (set a-set) elem))]
            (do (cons elem others))
          )
  )
)

(defn powerset [a-set]
  [:-])

