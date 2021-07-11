(ns recursion)

(defn product [coll]
  (if 
    (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) 
       (empty? (rest coll))))

(defn my-last [coll]
  (if
    (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if
    (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if
    (> (count seq-1) (count seq-2)) 
    seq-1
    seq-2
     ))

(defn longest-sequence [a-seq]
  (if 
    (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if
    (empty? a-seq)
    a-seq
    (if 
      (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq))) 
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq)
    false
    (== (first a-seq) elem)
    true
    :else
    (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
    ()
    ) 
  )

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (or (empty? a-seq) (empty? b-seq))
    false
    (== (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
    :else
    false))

(defn my-map [f seq-1 seq-2]
  
  (cond
    (or (empty? seq-1) (empty? seq-2))
    ()
    :else
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n)
    0
    (> 3 n)
    1
    :else
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
   (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq n]
  (if (zero? n)
    ()
    (cons 
      (seq a-seq) 
      (rotations-helper 
        (concat (rest a-seq) [(first a-seq)]) (dec n)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)]
      (if (freqs element)
        (my-frequencies-helper (update-in freqs [element] inc) (rest a-seq))
        (my-frequencies-helper (assoc freqs element 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '() 
    (let [first-entry (first a-map)]
      (concat 
        (repeat 
          (second first-entry) (first first-entry))
        (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
    '()
    (zero? n)
    (cons (first coll) (my-drop n (rest coll)))
    :else
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    [(my-take div a-seq) (my-drop div a-seq)]
    ))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
    b-seq
    (empty? b-seq)
    a-seq
    :else
    (let [a-first (first a-seq)] 
      (let [b-first (first b-seq)]
        (if (< a-first b-first)
          (cons a-first (seq-merge (rest a-seq) b-seq))
          (cons b-first (seq-merge a-seq (rest b-seq)))
          )))))

(defn merge-sort [a-seq]
  (cond 
    (empty? a-seq)
    '()
    (= (count a-seq) 1)
    a-seq
    :else
    (let [halved (halve a-seq)]
      (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

