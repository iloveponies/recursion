(ns recursion)

(defn product [coll]

  (if (empty? coll)
    1
    (* (first coll) (product (rest  coll)))
    )

  )


(defn singleton? [coll]

  (and (not (empty? coll)) (empty? (rest coll)))

  )

(defn my-last [coll]

  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
       (my-last (rest coll))
      )
    )

  )


(defn max-element [a-seq]

  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
       (max (first a-seq) (max-element (rest a-seq)))
      )

    )

  )

(defn seq-max [seq-1 seq-2]

  (if (< (count seq-1) (count seq-2))

    seq-2

    seq-1

    )

  )

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  [:-])

(defn my-filter [pred? a-seq]


    (cond
     (empty? a-seq) '()
     (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
     :else
     (my-filter pred? (rest a-seq))
     )

  )



(defn sequence-contains? [elem a-seq]

  (if (empty? a-seq)
    false
    (
     if (= elem (first a-seq))
     true
     (sequence-contains? elem (rest a-seq))
     )
    )

  )


(defn my-take-while [pred? a-seq]

  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   '()
    )

  )

(defn my-drop-while [pred? a-seq]

  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else
   a-seq
  )
  )


(defn seq= [a-seq b-seq]

  (cond
   (or ( and (empty? a-seq) (not (empty? b-seq)))
       ( and (empty? b-seq) (not (empty? a-seq)))
       ) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else
   false
   )


  )


(defn my-map [f seq-1 seq-2]

  (cond
   (empty? seq-1) '()
   (empty? seq-2) '()
   :else
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   )

  )


(defn power [n k]

  (if (== 0 k)
    1
    (* n
       (power n (dec k))
    )


  )
  )



(defn fib [n]

  (cond
   (== n 0) 0
   (== n 1) 1
   :else
   (+ (fib (- n 1)) (fib (- n 2)))
   )

  )


(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    )
  )

(defn my-range [up-to]

  (if (>= 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
    )

  )


(defn tails [a-seq]

  (if (empty? a-seq)
    '( () )
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))
    )

  )


(defn inits [a-seq]


  (map reverse (tails (reverse a-seq)))

  )




(defn rotations [a-seq]

  (let

    [ini (rest ( reverse (inits a-seq)))
     tail (rest (tails a-seq))
     cnt ( dec (count a-seq))
     arr (range 0 cnt)

     ]


    ((fn ! [subini subtail ]

       (if (empty? subini)
         '()
         (cons (concat (first subtail) ( first subini)) ( ! (rest subini) (rest subtail)))
         )


       ) ini tail )


    )

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

