(ns recursion)

(defn product [coll]
    (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll))))
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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll)))
)

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq))))
  )



(defn which-is-longer [seq-1 seq-2]
  (cond
   (empty? seq-1) 2
   (empty? seq-2) 1
   :else (which-is-longer (rest seq-1) (rest seq-2))
   )
)


(defn seq-max [seq-1 seq-2]
  (if (= (which-is-longer seq-1 seq-2) 1)
    seq-1
    seq-2)
  )
(defn longest-sequence [a-seq]
  (cond
   (empty? (rest a-seq))(first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
      )
  )

(defn my-filter [pred? a-seq]
  ;(filter pred? a-seq)
  (if (empty? a-seq)
    a-seq

    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (cond
      (= (first a-seq) elem) true
      :else (sequence-contains? elem (rest a-seq))
    )
  )
)

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()
  )
  )

(defn my-drop-while [pred? a-seq]
  ;(drop-while pred? a-seq)
  (cond
   (empty? a-seq) ()
   (not (pred? (first a-seq))) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))

  )


  )

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (empty? a-seq) false
   (empty? b-seq) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))
  )
  )

(defn my-map [f seq-1 seq-2]
  (cond
   (and (not (empty? seq-1)) (not (empty? seq-2)))
     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   :else ()
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
   :else (+ (fib (dec n)) (fib (dec (dec n)))
   )
  )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)
   )
  )
)

(defn my-range [up-to]
  (let [decadence (dec up-to)]
    (cond
     (= up-to 0) ()
     :else (cons decadence (my-range decadence))
    )
  )
)

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons a-seq (tails (rest a-seq)))
   )
  )

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))
  )
)
(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))
  )
)

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq))
   :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))

  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) []
   :else (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))
  )
)

(defn my-take [n coll]
  (cond
   (or (empty? coll) (= n 0)) ()
   :else (cons (first coll) (my-take (dec n) (rest coll)))
   )
)

(defn my-drop [n coll]
  (cond
   (empty? coll) ()
   (> n 0) (my-drop (dec n) (rest coll))
   :else (cons (first coll) (my-take (dec n) (rest coll)))
   )
)

(defn halve [a-seq]
   (let [halb (int (/ (count a-seq) 2))
        cnt (count a-seq)]
    (vector (my-take halb a-seq) (my-drop halb a-seq))
    )
)

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (let[a-val (first a-seq)
        b-val (first b-seq)]
     (cond
      (< a-val b-val) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      :else (cons (first b-seq) (seq-merge a-seq (rest b-seq))
     )
   )
  )
)
  )

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2) a-seq
   :else (let [[seq-1 seq-2] (halve a-seq)]
           (seq-merge (merge-sort seq-1) (merge-sort seq-2))
           )
   )
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


