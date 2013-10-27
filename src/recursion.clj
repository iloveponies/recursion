(ns recursion)

(defn product [coll]
  (if(empty? coll)
    1
    (* (first coll)
      (product (rest coll))
    )
  ))

(defn singleton? [coll]
   (if (= 2
     (if(empty? coll)
        0
        (+ 1       (product (rest coll)))
       ))
         true
         false

  ))


(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
   (my-last (rest coll))))



(defn max-element [a-seq]

  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
       (max-element (rest a-seq))))

  )
(defn seq-count [seq-1]
  (if (empty? seq-1)
    0
    (+ 1
       (seq-count (rest seq-1)))))


(defn seq-max [seq-1 seq-2]
  (if (> (seq-count seq-1) (seq-count seq-2))
         seq-1
         seq-2
         )
  )


(defn longest-sequence [a-seq]
   (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
            (longest-sequence (rest a-seq))
  )))




(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons ( first a-seq)(my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
      )


    ))




(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
   true

   :else
     (sequence-contains? elem (rest a-seq)) ))

 (defn filter-until2 [pred? a-seq]
   (or (empty? a-seq) (not (pred? (first a-seq)))
   ))

  (defn filter-until [pred? a-seq]

  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (if(pred? (first a-seq))
      (cons ( first a-seq)(filter-until pred? (rest a-seq)))
      ;(filter-until pred? (rest a-seq))
      )
    )

     )

  (defn last-seq [pred? a-seq]
    (if (pred? (first a-seq))
     a-seq
     ()
     )
    )

(defn my-take-while [pred? a-seq]

   (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (if(pred? (first a-seq))
      (cons ( first a-seq)(filter-until pred? (rest a-seq)))
      ;(filter-until pred? (rest a-seq))
      )
    )
 )







(defn my-drop-while [pred? a-seq]
(if (or (empty? a-seq))
    ()
    (if(pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
       a-seq)
      )
    )

  (defn seq2= [a-seq b-seq]
     ;(not (and (
    (not(and (not-any? nil? a-seq) (not-any? nil? b-seq))))



(defn seq= [a-seq b-seq]
   (cond
   (not(and (not-any? nil? a-seq) (not-any? nil? b-seq))) false
    (and (empty? a-seq) (empty? b-seq))  true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else
    false))



(defn my-map [f seq-1 seq-2]
   (if (empty? seq-2)
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
     ))
(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
  (= n 0 ) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))
  ))

(defn my-repeat [how-many-times what-to-repeat]

  (if (>= 0 how-many-times)
    ()
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat ))

    ))


(defn my-range [up-to]
  (if (= 0 up-to)
    ()
     (cons  (dec up-to) (my-range (dec up-to) ))
  ))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq a-seq)
    (cons a-seq (tails (rest a-seq) ))
    ))


(defn inits [a-seq]

  (if (empty? a-seq)
     (cons a-seq a-seq)
    (cons a-seq (inits (reverse(rest(reverse a-seq))) ))
  ))


(defn clean [a-seq]
 (if (> 1 (count (first a-seq))) true false)
  )



(defn concatenation [a-seq b-seq]
(concat a-seq b-seq)
    )




(defn concatenation2 [a-seq b-seq]
  (if (empty? a-seq)
  ()
  (cons (concat  a-seq b-seq )
       (concatenation2 (rest a-seq)(concat b-seq (vector(first a-seq))  )))

  ))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())

(concatenation2 a-seq []))
  ;(assoc a-seq (dec(count a-seq)) (get a-seq 0))
 ; (cons  a-seq (concatenation2 (vector(rest a-seq)) (vector (first a-seq))))
  )

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))




(defn my-frequencies-helper [freqs a-seq a-set]
  (if (empty? a-set)
    ()
  (cons (count-elem-helper 0 (first a-set) a-seq) (my-frequencies-helper {} a-seq (rest a-set))))
    ;(count-elem-helper 0 (first a-set) a-seq)
  )
(defn my-frequencies [a-seq]
  ;(assoc {} (first(set a-seq)) (1 2))
 (zipmap (set a-seq) (my-frequencies-helper {} a-seq (set a-seq))))

  (defn sulutpois [a-seq b-seq]

    (concat a-seq b-seq)
    )

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
  (concat  (my-repeat (val(first a-map)) (key(first a-map)))
         (un-frequencies (rest a-map)))


  )
  )

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))
  ))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if ( = 0 n)
      coll
    (my-drop (dec n)(rest coll)))
      )
    )


(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq)
     (my-drop (int (/ (count a-seq) 2)) a-seq))
  )

(defn oma-merge [a-seq b-seq]

  (if (empty? b-seq)
      a-seq
      (if (< (first a-seq) (first b-seq))
       (concat (vector (first a-seq))  b-seq)
       (concat (vector (first b-seq)) (oma-merge a-seq (rest b-seq))))))



(defn seq-merge [a-seq b-seq]
    (if (empty? a-seq)
      b-seq
         (seq-merge (rest a-seq) (oma-merge  (vector(first a-seq))  b-seq))

            ))

  ;(if (empty? a-seq)
  ;    ()
   ;   (seq-merge (rest a-seq) b-seq)



(defn halve-kahdeksi-vektoriksi [a-seq]

  (first (halve a-seq))

  )


(defn merge-sort [a-seq]
      (if (or (empty? a-seq) (= 1 (count a-seq)))
        a-seq
     (seq-merge (merge-sort(get (halve a-seq) 0)) (merge-sort(get (halve a-seq) 1)) )
   )

)
(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])








