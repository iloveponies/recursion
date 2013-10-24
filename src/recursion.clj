(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
  )
)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)
  )
)

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (= (my-last(rest coll)) nil)
      (first coll)
      (my-last (rest coll))
    )
  )
)

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (<= (max (first a-seq) (max-element (rest a-seq))) (first a-seq))
        (first a-seq)
        (max-element (rest a-seq))
      )
    )
  )
)

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1)
)

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (= (seq-max (first a-seq) (longest-sequence (rest a-seq))) (first a-seq))
      (first a-seq)
      (longest-sequence (rest a-seq))
    )
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)            false
    (= (first a-seq) elem)    true
    :else                     (sequence-contains? elem (rest a-seq))
  )
)

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)         a-seq
   (pred? (first a-seq))  (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else                  ()
  )
)

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)             a-seq
    (pred? (first a-seq))      (my-drop-while pred? (rest a-seq))
    :else                      a-seq
   )
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))  true
    (and (or (empty? a-seq) (empty? b-seq))
         (or ((complement empty?) a-seq) ((complement empty?) b-seq))
    )                                    false
    (= (first a-seq) (first b-seq))      (seq= (rest a-seq) (rest b-seq))

    :else                                false
   )
)

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))
  )
)

(defn power [n k]
  (cond
    (= k 0)       1
    :else         (* n (power n (dec k)))
  )
)

(defn fib [n]
  (cond
    (== n 0)       0
    (== n 1)       1
    :else          (+ (fib (dec n)) (fib (dec(dec n))))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1)     ()
    :else                    (cons what-to-repeat(my-repeat (dec how-many-times) what-to-repeat) )
  )
)

(defn my-range [up-to]
  (cond
    (<= up-to 0)       ()
    :else              (cons (dec up-to) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (cond
   (empty? a-seq)      (cons () a-seq)
   :else               (cons (seq a-seq) (tails (rest a-seq)))
  )
)

(tails [1 2])

(defn inits [a-seq]
  (cond
    (empty? a-seq)      (cons () a-seq)
    :else               (rest(cons () (reverse(map reverse (tails(reverse a-seq))))))
  )
)







(defn rotation-helper [ n a-seq]
  (let [ rot (fn [seq-1] (reverse(rest(reverse(cons(last a-seq) a-seq)))))]
    (cond
      (== n (count a-seq))     a-seq
      ; (== n 0)                 (cons (rotation-helper (inc n) (rot a-seq)) () )
      :else                    (cons (rot a-seq) (rotation-helper (inc n) (rot a-seq)))
    )
  )
)

(defn rotations [a-seq]
  (rotation-helper 0 a-seq)
)








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

