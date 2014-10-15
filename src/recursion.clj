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

  (if (empty? a-seq)
    freqs
    (let

      [newfr (my-frequencies-helper freqs (rest a-seq))]

      (if (contains? newfr (first a-seq))
        (assoc newfr (first a-seq) (+ 1 (get newfr (first a-seq)) ))
        (assoc newfr (first a-seq) 1)
        )

      )
    )

  )

(defn my-frequencies [a-seq]

  (my-frequencies-helper {} a-seq)

 )


(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]

  (apply concat (
   map
   (fn [[x y]]

     (repeat y x)

     )
   a-map
   ))

  )

(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]

  ((fn ! [k n coll]
     (if (or (> k n) (empty? coll))
       '()
       (cons (first coll) (! (inc k) n (rest coll)))
       )
     ) 1 n coll)

  )


(defn my-drop [n coll]

  ((fn ! [k n coll]

     (cond
      (empty? coll) '()
      (> k n) coll
      :else
      (! (inc k) n (rest coll))
      )
     ) 1 n coll)

  )

(defn halve [a-seq]

  (let

    [mid (int (/ (count a-seq) 2))]

    (conj [] (my-take mid a-seq) (my-drop mid a-seq) )

    )

  )

(defn seq-merge [a-seq b-seq]

  (
   (fn ! [as bs]

     (cond

      (and (empty? as) (empty? bs)) '()
      (empty? as) bs
      (empty? bs) as
      :else
      (let

        [l (first as) r (first bs)]
        (if (< l r )

          (cons l (! (rest as) bs))

          (cons r (! as (rest bs)))
          )
        )

      )

     )

   a-seq b-seq)

  )


(defn merge-sort [a-seq]



      (if (>= 1 (count a-seq))

        a-seq

        (
         let
         [ [x y] (halve a-seq)]

         (seq-merge (merge-sort x) (merge-sort y))

         )

        )


  )

(defn monotonic? [a-seq]


  (and (not (empty? a-seq )) (or (apply <= a-seq) (apply >= a-seq)))

  )


(defn split-into-monotonics [a-seq]

  (if (empty? a-seq)
    '()
    (let
    [rev (rest (reverse (inits a-seq)))
     f (my-take-while monotonic? rev)
     total (first (drop (- (count f) 1) f))
     restseq (drop (count f) a-seq)
     ]


       (cons total (split-into-monotonics restseq))


    )
)
)



(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

