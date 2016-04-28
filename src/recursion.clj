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
  (letfn [(rot [h t]
               (if (empty? h)
                 nil
                 (cons (concat h t) (rot (rest h) (concat t [(first h)]  ) ) )
               ))]

     (rot a-seq [])
    ))

(defn my-frequencies-helper [freqs a-seq]
  (let [ f (first a-seq)]
  (cond
   (empty? a-seq)
     freqs
   (boolean(freqs (first a-seq)))
     ( my-frequencies-helper (assoc freqs f (+(freqs f ) 1)) (rest a-seq))
   :else
     ( my-frequencies-helper (assoc freqs f  1) (rest a-seq))
    )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
   (flatten (map (fn [x] (repeat (second x) (first x) )) a-map)) )

(defn my-take [n coll]
  (if(or(>= 0 n)(empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))
    ))

(defn my-drop [n coll]
  (if(or(>= 0 n)(empty? coll))
    coll
    (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let[ c (count a-seq)
        h (int (/ c 2))]
    [(my-take h a-seq) (my-drop  h a-seq)]
    ))

(defn seq-merge [a-seq b-seq]
 (letfn [(seq-merge2 [a-seq b-seq res]
  (cond
  (or (empty? a-seq) (empty? b-seq))
    (concat res a-seq b-seq)
  (> (first a-seq) (first b-seq))
    (seq-merge2 a-seq (rest b-seq) (concat res [(first b-seq)]))
  :else
    (seq-merge2 (rest a-seq) b-seq (concat res [(first a-seq)]))
    ))]
   (seq-merge2 a-seq b-seq [])
   ))

(defn merge-sort [a-seq]
  (let[ [d-seq e-seq] (halve a-seq)]
  (if(<=(count a-seq) 1)
     a-seq
    (seq-merge (merge-sort d-seq)(merge-sort e-seq))
    )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

