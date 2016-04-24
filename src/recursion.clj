(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (empty?(rest coll))
    (not(empty? coll))
    ))

(defn my-last [coll]
  (if (or(singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))
           ))

(defn max-element [a-seq]
  (if (or(singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) ( max-element(rest a-seq)) )
    ))

(defn seq-max [seq-1 seq-2]
  (if(>(count seq-1) (count seq-2))
    seq-1
    seq-2
  ))

(defn longest-sequence [a-seq]
  (if (or(singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) ( longest-sequence(rest a-seq)) )
    ))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
           (cons (first a-seq)
            (my-filter pred? (rest a-seq))
            )
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq ]
 (letfn [(mtw [pred? a-seq res]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (mtw pred? (rest a-seq) (conj res (first a-seq)  ) )

   :else
      res))]
   (reverse(mtw pred? a-seq ()))
  ))



(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq)
  )

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1)(first seq-2) ) (my-map f (rest seq-1)(rest seq-2)))

  ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+  (fib (- n 1)) (fib (- n 2))

      )))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
      ))

(defn my-range [up-to]
 (if (zero? up-to)
    []
    (cons (dec up-to) (my-range(dec up-to)) )
      ))

(defn tails [a-seq]
  (if (empty? a-seq)
     [a-seq]
    (cons  a-seq (tails (rest a-seq)))
  ))

(defn inits [a-seq]
   (map reverse (tails  (reverse a-seq))))

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

