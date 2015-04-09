(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))

)

(defn singleton? [coll]
   (if (empty? coll) false
     (
  empty?  (rest coll)
     )
  )

)

(defn my-last [coll]
   (if ( singleton? coll) (first coll)

    ( if (empty? coll) nil  (my-last (rest coll))  )
   )
)

(defn max-element [a-seq]
    (if (empty? a-seq) nil

      ( if ( singleton? a-seq) (first a-seq)   (max (first a-seq)  (max-element (rest a-seq)) )  )
    )
)




(defn seq-max [seq-1 seq-2]
 (if (> (count seq-1 ) (count seq-2) )  seq-1 seq-2)
)




(defn longest-sequence [a-seq]

   (if (empty? a-seq) nil

  ( if ( singleton? a-seq) (first a-seq) (seq-max (first a-seq)  (longest-sequence  (rest a-seq)) )  )
  )

)




(defn my-filter [pred? a-seq]
  ( if (empty? a-seq) a-seq

(if (pred? (first a-seq))

       (cons (first a-seq) (my-filter pred? (rest a-seq)) )
       (my-filter pred? (rest a-seq)))     )


)

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
   false

   (= elem (first a-seq))
   true

   :else
   (recur elem (rest a-seq))
   )

  )



(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)               ()
    (not (pred? (first a-seq)))  ()
    :else (cons (first a-seq)    (my-take-while pred? (rest a-seq))))
)



(defn my-drop-while [pred? a-seq]
   (cond
     (empty? a-seq)   a-seq
    (not (pred? (first a-seq)))  a-seq
    :else   (my-drop-while pred? (rest a-seq)))
)

(defn seq= [a-seq b-seq]
   (cond
  (and (empty? a-seq) (empty? b-seq))  true

   (or  (empty? a-seq) (empty? b-seq))  false

   (not (= (first a-seq) (first b-seq))) false

   :else (recur (rest a-seq) (rest b-seq)))

)

(defn my-map [f seq-1 seq-2]

   (cond

  (or (empty? seq-1) (empty? seq-2)) ()

  :else (cons (f  (first seq-1 ) (first seq-2 )  )    ( my-map f (rest seq-1)  (rest seq-2)   )  )

    )
)





(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)  )))


  )

(defn fib [n]
  (if ( = n 0 ) 0  (if (= n 1) 1    (+ (fib (- n 1)) (fib (- n 2))  )    ) )

)


(defn my-repeat [how-many-times what-to-repeat]
    (if (>= 0 how-many-times )
()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))

)




(defn my-range [up-to]
     (let [abc (dec up-to)]

  (if ( >= 0 up-to) ()
    (cons abc (my-range (dec up-to)     ) )
  )

  ))



(defn tails [a-seq]
  (if (empty? a-seq)
     (cons a-seq ()) (cons (seq a-seq) (tails (rest a-seq))))
)

(defn inits [a-seq]


   (reverse (map sort (map reverse (tails (reverse a-seq)))))


)

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs

      (frequencies a-seq)
  )
)

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq)
)



(defn un-frequencies [a-map]
  (if (empty? a-map) {}


           (concat (my-repeat (get(first a-map) 1) (get(first a-map) 0)  ) (un-frequencies (rest a-map) ) )
  )
)




(defn my-take [n coll]

(if (or (empty? coll) (zero? n) )   ()  (cons (first coll) (my-take (dec n) (rest coll))))
)
(defn my-drop [n coll]
   (if(not (zero? n) )
    (recur (dec n) (rest coll)) coll))

(defn halve [a-seq]
  (cond


  (= (count a-seq) 1)  (assoc (assoc [] 0 () ) 1 (cons (get a-seq 0 ) () )  )
   :else
   (assoc (assoc [] 0 (my-take (int (/ (count a-seq) 2)) a-seq) )

     1  (my-drop (int (/ (count a-seq) 2)) a-seq  )  )
  )

)

(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq ))
)

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq) ) a-seq
   (apply seq-merge (map merge-sort (halve a-seq)))
  )
)


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

