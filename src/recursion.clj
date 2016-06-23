(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (not (empty? (rest coll)))
    (my-last (rest coll))
    (first coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq)))
     )
   ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
   (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))
     )
   ))

(defn my-filter [pred? a-seq]
  (if(empty? a-seq)
    ()
    (if(pred? (first a-seq))
         (cons (first a-seq) (my-filter pred? (rest a-seq)))
         (my-filter pred? (rest a-seq)))))
    
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (and (number? (first a-seq)) (== (first a-seq) elem ))
      true
    (= (first a-seq) elem )
      true
    :else
      (sequence-contains? elem (rest a-seq)))
  )

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) ( my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
       a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
       false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2) )
      ()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))
    )

(defn my-range [up-to]
  (if(< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to))))
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq))))
  )

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits  a-seq))))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          count (if (contains? freqs item)
                  (inc (get freqs item))
                  1)
          freqs-new (assoc freqs item count)]
      (my-frequencies-helper freqs-new (rest a-seq)))
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (let [item (first a-map)] (repeat (val item) (key item))) (un-frequencies (rest a-map))))
  )

(defn my-take [n coll]
  (cond 
    (>= n (count coll))
      (seq coll)
    (== n 0)
      ()
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))
    )
  )

(defn my-drop [n coll]
  (cond 
    (>= n (count coll))
      ()
    (== n 0)
      coll
    :else
      (my-drop (dec n) (rest coll))
    )
  )

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))
        first-half (my-take n a-seq)
        second-half (my-drop n a-seq)]
    (cons first-half (cons second-half ())))
  )

(defn seq-merge [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq))
      ()
    (and (empty? a-seq) (not (empty? b-seq)))
      b-seq
    (and (empty? b-seq) (not (empty? a-seq)))
      a-seq
    :else
      (if(>= (first a-seq) (first b-seq)) 
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
      )
  )

(defn merge-sort [a-seq]
  (if(< (count a-seq) 2)
      a-seq
      (let [halves (halve a-seq)]
        (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))
    )
  )

(defn monotonic? [a-seq]
  (if(< (count a-seq) 3)
    true
    (or (= a-seq (merge-sort a-seq)) (= a-seq (reverse (merge-sort a-seq)))))
  )

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [elem (first (filter monotonic? (inits a-seq)))]
      (cons elem (split-into-monotonics (drop (count elem) a-seq)))))
  )

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (= 1 (count a-set)) (list a-set)
    :else (for [elem1 a-set
                rest-elem (permutations (disj (set a-set) elem1))]
            (cons elem1 rest-elem))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{(set a-set)}
    (apply clojure.set/union
          #{(set a-set)}
          (map (fn [x] (powerset (disj (set a-set) x))) (set a-set))))
  )
  

