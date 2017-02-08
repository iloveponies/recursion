(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll ))
       (= (rest coll) '())
  )
)


(defn my-last [coll]
  (if (= (rest coll) '())
      (first coll)
      (my-last (rest coll))
  )
)

(defn max-element [a-seq]
  (if (= (rest a-seq) '())
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq)))
  )
)

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2
  )
)

(defn longest-sequence [a-seq]
  (if (= (rest a-seq) '())
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  )
)


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq)
              (my-filter pred? (rest a-seq))
        )
        (my-filter pred? (rest a-seq))
    )
  )
)


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))
  )
)

(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq)
       a-seq
     (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else
       '()
   )
)

(defn my-drop-while [pred? a-seq]
  (cond
     (empty? a-seq)
       a-seq
     (pred? (first a-seq))
       (my-drop-while pred? (rest a-seq))
     :else
       a-seq
  )
)

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq))
       true
     (or  (empty? a-seq) (empty? b-seq))
       false
     (= (first a-seq) (first b-seq))
       (seq= (rest a-seq) (rest b-seq))
     :else
       false
  )
)

(defn my-map [f seq-1 seq-2]
  ;(print seq-1,"..", seq-2, \n)
  (cond
     (or (empty? seq-1) (empty? seq-2))
       '()
     (or (singleton? seq-1) (singleton? seq-2))
       ;(print 2)
       (list (f (first seq-1) (first seq-2)))
     :else
       (cons (f (first seq-1) (first seq-2))  (my-map f (rest seq-1) (rest seq-2)))
  )
)


(defn power [n k]
  (if (= k 0)
      1
      (* n (power n (dec k)))
  )
)


(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))
  )
)


(defn my-repeat [how-many-times what-to-repeat]
  (if (<=  how-many-times 0)
       '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat ))
  )
)

(defn my-range [up-to]
  (if (= up-to 0)
       '()
       (cons (dec up-to) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (if (empty? a-seq)
      '([])
      (cons  a-seq (tails (apply vector (rest a-seq))))
  )
)


(defn inits [a-seq]
  (map reverse (tails (reverse a-seq)))
)

(defn rotations [a-seq]
  (apply vector (set
    (map concat  (reverse (tails a-seq)) (inits a-seq))
                )
  )

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

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

(defn get-freqs [freqs elem]
  (if (contains? freqs elem)
      (get freqs elem)
      0
  )
)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (assoc freqs (first a-seq ) (inc (get-freqs freqs (first a-seq))))]
         (my-frequencies-helper new-freqs (rest a-seq))
    )
  )
)


(defn my-frequencies [a-seq]
  (my-frequencies-helper '{} a-seq )
)

(defn un-frequencies [a-map]
  (if (empty? a-map)
      []
      (concat (repeat (second (first a-map)) (key (first a-map)) ) (un-frequencies (rest a-map)))
  )
)


(defn my-take [n coll]
   (if (or (<= n 0) (empty? coll))
       '()
       (cons (first coll) (my-take (dec n) (rest coll)))
    )
)

(defn my-drop [n coll]
  (if (<= n 0)
       coll
       (my-drop (dec n) (rest coll))
  )
)

(defn halve [a-seq]
    (let [mlength (int (/ (count a-seq) 2))]
      (vector (my-take mlength a-seq) (my-drop mlength a-seq))
    )
)

(defn seq-merge [a-seq b-seq]
   (let [a1 (first a-seq)
         b1 (first b-seq)]
      (cond
        (nil? a1)
          b-seq
        (nil? b1)
          a-seq
        (< a1 b1)
          (cons a1 (seq-merge (rest a-seq) b-seq))
        :else
          (cons b1 (seq-merge a-seq (rest b-seq)))
      )
   )
)


(defn merge-sort [a-seq]
   (if (or (singleton? a-seq) (empty? a-seq))
       a-seq
       (apply seq-merge (map merge-sort (halve a-seq)))
    )
)

(defn monotonic? [a-seq]
   (if (or (empty? a-seq) (singleton? a-seq))
       true
       (apply = (map (fn [x] (apply > x)) (map list a-seq (rest a-seq))))
   )
)

(defn inits_2 [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
)

(defn split-into-monotonics [x]
  )

(defn split-into-monotonics-helper [pares par_anterior]
   (let [primer_par (first pares)
         init       (first primer_par)
         tail       (second primer_par)
         ]
     (cond
        (empty? primer_par)
           (list (first par_anterior))
        (monotonic? init)
           (split-into-monotonics-helper (rest pares) primer_par)
        :else
           (cons (first par_anterior) (split-into-monotonics (second par_anterior) ))
     )
   )
)

(defn split-into-monotonics [a-seq]
   (let [ pares (map vector (inits_2 a-seq) (tails a-seq))]
      (split-into-monotonics-helper pares '())
   )
)



(defn multiply [element permutation]
  (map concat (inits_2 permutation) (repeat (inc (count permutation)) (list element)) (tails permutation))
)

(defn permutations [a-set]
  (cond
     (empty? a-set)
        '(())
     :else
       (apply concat (map (fn [permutation] (multiply (first a-set) permutation)) (permutations (rest a-set))))
      ; (apply concat (map (fn [x] (multiply 4 x)) '([2 3] [3 2])))

  )
)

(defn add_element [e p-set]
   (concat (map (fn [p] (conj p e)) p-set) p-set)
)


(defn powerset [a-set]
  (cond
     (empty? a-set)
        '(#{})
     :else
        (add_element (first a-set) (powerset (rest a-set)))
  )
)

