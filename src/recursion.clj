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
      false))
  )

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll)))
  )

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
   (first a-seq)
   (max (first a-seq)
        (max-element (rest a-seq))))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
   ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq))))
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))
    )
  )

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not (= (first a-seq) elem))
     (sequence-contains? elem (rest a-seq))
   (= (first a-seq) elem) true
   )
  )

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))
      ()
      )
    )
 )

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq
   )
  )

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
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
  (if (== k 0)
    1
    (* n (power n (- k 1)))
    )
  )

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))
   )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))
    )
  )

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (- up-to 1)
          (my-range (- up-to 1)))
    )
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons a-seq
          (tails (rest a-seq)))
    )
  )

(defn inits [a-seq]
  (reverse (tails a-seq))
  )

(defn rotations [a-seq] 'WIP
  (if (empty? a-seq)
    ()

    (cons
     (first a-seq)
     (rotations (rest a-seq)))
    )
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs
          (if (contains? freqs (first a-seq))
            (update-in freqs [(first a-seq)] inc)
            (conj freqs {(first a-seq) 1})
            )]
      (my-frequencies-helper new-freqs (rest a-seq))
      )
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies-helper [seq item]
  (if (= 0 (get item 1))
    seq
    (let [new-seq (cons (get item 0) seq)]
     (un-frequencies-helper new-seq [(get item 0) (dec (get item 1))]))
    )
  )

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (un-frequencies-helper () (first a-map))
          (un-frequencies (rest a-map)))
    )
  )

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    ()
    (cons (first coll)
           (my-take (dec n) (rest coll)))
    )
  )

(defn my-drop [n coll]
  (if (or (== n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll)))
  )

(defn halve-helper [n a-seq]
  (if (or (== n 0) (empty? a-seq))
    (cons a-seq ())
    (cons
     (first a-seq)
     (halve-helper (dec n) (rest a-seq)))
    )
  )

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
   (cons
    (my-take n a-seq)
    (cons (my-drop n a-seq) ()))
    )
  )

(defn seq-merge [a-seq b-seq]
  (let [next-seq (cond
                   (and (empty? a-seq) (empty? b-seq)) 'n'
                   (empty? a-seq) 'b'
                   (empty? b-seq) 'a'
                   (< (first a-seq) (first b-seq)) 'a'
                   :else 'b')]
    (cond
     (= next-seq 'a') (cons (first a-seq)
                            (seq-merge (rest a-seq) b-seq))
     (= next-seq 'b') (cons (first b-seq)
                            (seq-merge a-seq (rest b-seq)))
     :else ())
    )
  )

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge
     (merge-sort (first (halve a-seq)))
     (merge-sort (second (halve a-seq))))
    )
  )

(defn monotonics-inc-helper [n a-seq]
    (cond
     (< (count a-seq) 2) (+ n (count a-seq))
     (< (second a-seq) (first a-seq)) n
     :else (monotonics-inc-helper (inc n) (rest a-seq))
    )
  )

(defn monotonics-dec-helper [n a-seq]
   (cond
     (< (count a-seq) 2) (+ n (count a-seq))
     (> (second a-seq) (first a-seq)) n
     :else (monotonics-dec-helper (inc n) (rest a-seq))
    )
  )

(defn split-into-monotonics-helper [n a-seq]
    (cond
     (< (count a-seq) 2) (+ n (count a-seq))
     (== (first a-seq) (second a-seq)) (split-into-monotonics-helper (inc n) (rest a-seq))
     (< (second a-seq) (first a-seq)) (monotonics-dec-helper (inc n) (rest a-seq))
     :else (monotonics-inc-helper (inc n) (rest a-seq))
     )
    )


(defn split-into-monotonics [a-seq]
  (let [n (split-into-monotonics-helper 1 a-seq)]
    (if (empty? a-seq)
      ()
      (cons
       (my-take n a-seq)
       (split-into-monotonics (my-drop n a-seq))
       )
      )
    )
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

