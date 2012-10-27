(ns recursion)

(defn product [coll]

  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))

  )

(defn singleton? [coll]

   (and ((complement nil?) (first coll)) (empty? (rest coll)))

  )

(defn my-last [coll]

  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll)))

  )

(defn max-element [a-seq]

  (if (or (nil? a-seq) (empty? a-seq))
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (empty? (filter (fn [x] (< (first a-seq) x)) a-seq))
        (first a-seq)
        (max-element (filter (fn [x] (< (first a-seq) x)) a-seq))


      )))

  )

(defn seq-max [seq-1 seq-2]

  (cond
    (> (count seq-1) (count seq-2)) seq-1
  	:else seq-2

    )

  )

(defn longest-sequence [a-seq]

  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (longest-sequence(rest a-seq)) (first a-seq))
  	)
  )

(defn my-filter [pred? a-seq]

  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq)))


  )

(defn sequence-contains? [elem a-seq]


  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
   	:else (sequence-contains? elem (rest a-seq)))

  )

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? first(a-seq)) (cons (first a-seq) my-take-while((rest a-seq)))
    :else nil)
  )

(defn my-drop-while [pred? a-seq]

	(cond
     (empty? a-seq) a-seq
     (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
     :else a-seq)
  )

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    :else (seq= (rest a-seq) (rest b-seq)))

  )

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) ()
    (empty? seq-2) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))


  ))

(defn power [n k]
  (cond
    (= k 0) 1
    (= k 1) n
    :else (* n (power n (- k 1))))

  )

(defn fib [n]
  (cond 
   (= n 0) 0
   (<= n 2) 1
   :else (+ (fib (- n 1)) (fib (- n 2))))

  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (>= 0 how-many-times) ()
   :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))

   ))

(defn my-range [up-to]
  (cond
   (= up-to 0) ()
   :else (cons (dec up-to) (my-range (dec up-to)))
  ))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons a-seq ())
   :else (cons a-seq (tails (rest a-seq)))
   ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]

  (cond
   (empty? a-seq) (cons () a-seq)
   :else (cons a-seq (rotate (count a-seq) a-seq)))


  )

(defn rotate [count a-seq]

  (let  [kaannetty (cons (rest a-seq) (first a-seq))]
  ;(cond
   ;(= 1 count) kaannetty
   ;:else (cons kaannetty (rotate (dec count) kaannetty) ))


  ))

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