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
     (empty? (rest coll))
  )
)

(defn my-last [coll]
   (cond
      (empty? coll) nil
      (singleton? coll) (first coll)
      :else (my-last (rest coll))
    )
)

(defn max-element [a-seq]
   (cond
      (empty? a-seq) nil
      (singleton? a-seq) (first a-seq)
      :else (max (first a-seq) (max-element (rest a-seq)))
    )
)

(defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2))
        seq-1
        seq-2
     )
)

(defn longest-sequence [a-seq]
    (cond 
       (empty? a-seq) nil
       (singleton? a-seq) (first a-seq)
       :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
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
       (empty? a-seq) false
       (= elem (first a-seq)) true
       :else (sequence-contains? elem (rest a-seq))
     )
)

(defn my-take-while [pred? a-seq]
     (cond
      (empty? a-seq) '()
      (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      :else '()
     )
)

(defn my-drop-while [pred? a-seq]
     (cond
      (empty? a-seq) '()
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
        '()
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
     )
)

(defn power [n k]
   (cond
     (= k 0) 1
     (= k 1) n
     :else (* n (power n (dec k)))
   )
)

(defn fib [n]
   (cond
      (= n 0) 0
      (= n 1) 1
      :else (+ (fib (- n 1)) (fib (- n 2))) 
   )
)

(defn my-repeat [how-many-times what-to-repeat]
   (if (> how-many-times 0)
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
      '()
   )
)

(defn my-range [up-to]
   (if (= up-to 0)
      '()
      (cons (dec up-to) (my-range (dec up-to)))
   )
)

(defn tails [a-seq]
   (if (= (count a-seq) 0)
       [[]]
       (cons (vec a-seq) (tails (rest a-seq)))
   )
)

(defn inits [a-seq]
  (defn inits-helper [a-seq]
      (if (= (count a-seq) 0)
          [[]]
          (cons (vec (reverse a-seq)) (inits-helper (rest a-seq)))
      )
  )
  (inits-helper (reverse a-seq))
)

(defn rotations [a-seq]
   (defn rotations-helper [n a-seq]
       (if (> n 0)
         (cons (seq a-seq) 
            (rotations-helper (dec n) (conj (vec (rest a-seq)) (first a-seq)))
         )
         '()
       )
   )
   (if (> (count a-seq) 0)
       (rotations-helper (count a-seq) a-seq)
       '(())
   )
)

(defn my-frequencies-helper [freqs a-seq]
   (if (> (count a-seq) 0)
       (my-frequencies-helper 
       (if (contains? freqs (first a-seq))
            (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
            (assoc freqs (first a-seq) 1)
       ) 
       (rest a-seq))
       freqs
   )
)

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
   (if (> (count a-map) 0)
       (concat 
          (repeat (get (first a-map) 1) (get (first a-map) 0))
          (un-frequencies (rest a-map))
       )
   )
)

(defn my-take [n coll]
   (if (and (> n 0) (> (count coll) 0))
       (cons (first coll) (my-take (dec n) (rest coll)))
       '()
   )
)

(defn my-drop [n coll]
   (if (> (count coll) n)
      (reverse (my-take (- (count coll) n) (reverse coll)))
      '()
   )
)

(defn halve [a-seq]
   (let [n (int (/ (count a-seq) 2))]
     [(my-take n a-seq) (my-drop n a-seq)]
   )
)

(defn seq-merge [a-seq b-seq]
  (defn seq-merge-helper [ab-seq a-seq b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq)) ab-seq
      (empty? a-seq) (concat ab-seq b-seq)
      (empty? b-seq) (concat ab-seq a-seq)
      :else 
        (if (<= (first a-seq) (first b-seq))
          (seq-merge-helper (concat ab-seq [(first a-seq)]) (rest a-seq) b-seq)
          (seq-merge-helper (concat ab-seq [(first b-seq)]) a-seq (rest b-seq))
        )
    )
  )
  (seq-merge-helper '() a-seq b-seq)
)

(defn merge-sort [a-seq]
   (if (> (count a-seq) 1)
      (seq-merge (merge-sort (vec (get (halve a-seq) 0)))
                 (merge-sort (vec (get (halve a-seq) 1)))
      )
     a-seq
   )
)

(defn split-into-monotonics [a-seq]
   (defn monotonic? [a-seq]
      (or (apply <= a-seq)
          (apply >= a-seq))
   )
   (defn split-helper1 [a-seq]
      (first (reverse (take-while monotonic? (drop-while empty? (reverse (inits a-seq))))))
   )
   (if (empty? a-seq)
       a-seq
       (let [prefix (split-helper1 a-seq)]
          (cons prefix (split-into-monotonics (drop (count prefix) a-seq)))
       )
   )
)

(defn permutations [a-set]
   (if (empty? a-set)
      a-set
      (concat (first a-set) (permutations (rest a-set)))
   )
)

(defn powerset [a-set]
  [:-])