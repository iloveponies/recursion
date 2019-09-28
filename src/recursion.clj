(ns recursion)
(defn product [coll]
 (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )
(defn singleton? [coll]
  (== 1 (reduce (fn [c coll] (inc c)) 0 coll))
  )

(defn my-last [coll]
(cond (next coll) (recur (next coll))
      :else (first coll)
      )
  )

(defn max-element [input]
(cond (empty? input) nil
      :else  (reduce #(if (> %1 %2) %1 %2) input)
      )
  )
(defn seq-max [seq-1 seq-2]
  (cond ( > (count seq-1) (count seq-2)) seq-1
      :else seq-2
  )
)
(defn swap [v i1 i2]
   (assoc v i2 (v i1) i1 (v i2)))

(defn longest-sequence-helper [n a-seq]
  (let [new-count (inc n)]
      (if (<= new-count (count a-seq))
            (cond (> (count (first a-seq)) (count (second a-seq))) (longest-sequence-helper new-count (rest (assoc a-seq new-count (first a-seq))))
                    :else (longest-sequence-helper new-count (rest a-seq))
       )
       (into [] (first (map seq a-seq)))
  )
)
)
(defn longest-sequence [a-seq]
   (cond (empty? a-seq) nil
         :else (longest-sequence-helper 0 a-seq)
         )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
      (if (pred? (first a-seq))
                     (cons (first a-seq) (my-filter pred? (rest a-seq)))
                     (recur pred? (rest a-seq)))
  )
)
(defn sequence-contains? [elem a-seq]
  (cond (first a-seq)
     (if(== elem (first a-seq))
     true
     (recur elem (rest a-seq))
    )
        :else false
    )
  )

(defn my-take-while [pred? a-seq]
(cond (empty? a-seq) '()
      :else   (cond (= false (pred? (first a-seq))) '()
                    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))
               )
             )
)

(defn my-drop-while [pred? a-seq]
   (if (empty? a-seq)
    '()
      (if (= false (pred? (first a-seq)))
                     (cons (first a-seq) (next a-seq))
                     (recur pred? (rest a-seq)))
  )

  )

(defn seq= [a-seq b-seq]
  (if (not(== (count a-seq) (count b-seq)))
    false
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false
  )
    )
)

(defn my-map [f seq-1 seq-2]
  (cond (empty? seq-2) '()
        :else  (if (first seq-2)
                     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
        )
        )
  )


(defn power [n k]
   (if (== 0 k)
        1
     (* n (power n (dec k))
     )
  )
)

(defn fib [n]
 (if (< n 2)
    n
    (+      (fib (- n 1))
            (fib (- n 2))
       )
)
)

(defn my-repeat [how-many-times what-to-repeat]

  (if (< how-many-times 1)
    (vector)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)  )
    )


 )

(defn my-range [up-to]
  (if (< up-to 1)
    (vector)
          (cons (Math/abs (- 1 up-to)) (my-range (dec up-to)))
    )
)

(defn tails [a-seq]
 (map #(drop % a-seq) (range (inc (count a-seq))))
  )

(defn inits [a-seq]
 (map #(take % a-seq) (range (inc (count a-seq))))
)

(defn rotations-helper [n a-seq]
  (cond (empty? a-seq) '(())
        :else (let [new-count (inc n)]
      (let [new-seq (concat (vector(last a-seq)) (reverse (drop 1 (reverse a-seq))))]

            (if (<= new-count (count a-seq))
          (cons new-seq (rotations-helper new-count new-seq))
           )
        )
  )
        )
)

(defn rotations [a-seq]
(rotations-helper 0 a-seq)
)

(defn my-frequencies-helper [freqs a-seq]
   (reduce (fn [m e]
              (assoc m e (inc (m e 0))))
          freqs a-seq)
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
  (if (first a-map)
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))
    )
  )

(defn my-take [n coll]
  (cond  (== n 0) '()
         (>= n (count coll)) (seq coll)
        :else (reverse (drop n (reverse coll)))
        )
  )

(defn  my-drop [n coll]
(cond (>= 0 n)
      (cond (= nil (seq coll)) coll
            :else (seq coll)
            )
      :else  ( my-drop (- n 1) (rest coll))
      )
  )

(defn halve [a-seq]
  (merge (vector (take (int (/ (count a-seq) 2)) a-seq)) (drop (int (/ (count a-seq) 2)) a-seq))
)
(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq))
  )

(defn merge-sort [a-seq]
  (cond
   (== 0 (count a-seq)) '()
   (== 1 (count a-seq)) a-seq
  :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))
    )
  )

(defn split-into-monotonics [a-seq]
 (if (and (first a-seq) (second a-seq))
  (cond (even? (count a-seq)) (cons (concat (vector (first a-seq)) (vector (second a-seq))) (split-into-monotonics (drop 2 a-seq)))
       :else (cons (concat (vector (first a-seq)) (vector (second a-seq)) (vector(second (rest a-seq)))) (split-into-monotonics (drop 3 a-seq)))
       )
  )
)
(defn permutations [a-set]
  (cond (empty? a-set) '(())
        :else   (let [a-seq (seq a-set)]
                  (concat (rotations a-seq) (rotations (reverse a-seq)))
                  )
        )
  )

(defn comb [k l]
  (if (= 1 k) (map vector l)
      (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2))
                    (comb (dec k) (drop (inc %1) l)))
              l))))

(defn powerset [a-set]
 ; (cons a-set (cond (empty? a-set) '(())
  ;      :else (cons (into #{} [(first a-set)]) (powerset (rest a-set)))
   ;     ))
   (apply concat
         (for [x (range 1 (inc (count a-set)))]
           (map #(into #{} %) (comb x a-set))))

  )
