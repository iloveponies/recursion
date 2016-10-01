(ns recursion)

(defn product [coll]
(if (empty? coll) 1
  (* (first coll) (product (rest coll))
  )
)
)

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false

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
(if (empty? a-seq) nil (apply max a-seq)
  )
)

(defn seq-max [seq-1 seq-2]
(if (> (count seq-1) (count seq-2)) seq-1 seq-2
  )
  )


(defn longest-sequence [a-seq]
 (if (empty? a-seq)
 nil
 (if (singleton? a-seq)
 (first a-seq)
 (seq-max (first a-seq) (longest-sequence (rest a-seq)))))
 )

(defn my-filter [pred? a-seq]
(cond
(empty? a-seq) a-seq
(pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
:else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
(cond
(empty? a-seq) false
(= (first a-seq) elem) true
:else (sequence-contains? elem (rest a-seq))
)
)

  (defn my-take-while [pred? a-seq]
(cond
(empty? a-seq) a-seq
(pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
:else '()))

(defn my-drop-while [pred? a-seq]
(cond
 (empty? a-seq) a-seq
  (not (pred? (first a-seq))) a-seq
  :else (my-drop-while pred? (rest a-seq)
)
  )
  )

 (defn seq= [a-seq b-seq]
 (cond
 (and (empty? a-seq) (empty? b-seq)) true
 (or (empty? a-seq) (empty? b-seq)) false
 (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
 :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2) ) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
  )
)

(defn power [n k]
(cond
  (== k 0) 1
  :else (* (* n 1) (power n (dec k))
  )
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
                '()
                (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
)
)

(defn my-range [up-to]
(cond
(== up-to 0) '()
  :else (cons (- up-to 1)(my-range (dec up-to)))
)
)


(defn tails [a-seq]
(cond
(empty? a-seq) (list ())
:else (cons a-seq (tails (rest a-seq)))
)
)

(defn inits [a-seq]
(map reverse ( reverse(tails (reverse a-seq)))
))

(defn rotations [a-seq]
(if (empty? a-seq)
'(())
(butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
(cond
(empty? a-seq) freqs
(contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1)) (rest a-seq))
:else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
)
)

(defn my-frequencies [a-seq]
(my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
(let [item (first a-map)]
(if (nil? item)
   '()
   (concat (repeat (second item) (first item)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
(cond
  (empty? coll) '()
  (> n 0) (cons (first coll) (my-take (- n 1) (rest coll)))
)
  )

 (defn my-drop [n coll]
   (if (or (= n 0)
           (empty? coll))
     coll
     (my-drop (dec n)
              (rest coll))))

 (defn halve [a-seq]
(let [second (int (/ (count a-seq) 2))
                  first  (- (count a-seq) second)]
(
                cons
                (my-take first a-seq)
                (my-drop first a-seq)

 )
)
)

 (defn halve [a-seq]
   (if (= (count a-seq) 1) (vector '() a-seq)
  (cons
    (my-take (int (/ (count a-seq) 2)) a-seq)
    (cons (my-drop (int (/ (count a-seq) 2)) a-seq) nil)))
)

   (defn seq-merge [a-seq b-seq]
(cond
(empty? a-seq) b-seq
(empty? b-seq) a-seq
(<= (first a-seq) (first b-seq)) (concat [(first a-seq)] (seq-merge b-seq (rest a-seq)))
:else (concat [(first b-seq)] (seq-merge a-seq (rest b-seq))))
  )

(defn merge-sort [a-seq]
(cond
(empty? a-seq) a-seq
(= (count a-seq) 1) a-seq
:else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))

)
)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

