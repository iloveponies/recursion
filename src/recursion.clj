(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll)))
  )

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll))))
  )

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (max (first a-seq) (max-element (rest a-seq)))))
  )

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1)
  )

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
      ))
  )

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq))))
  )

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ()))
  )

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq))
  )

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (empty? b-seq) (not (empty? a-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false)
  )

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (if (< k 1)
    1
    (* n (power n (+ k -1))))
  )

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (+ n -1)) (fib (+ n -2))))
  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) ()
    :else (cons what-to-repeat (my-repeat (+ how-many-times -1) what-to-repeat))
    )
  )

(defn my-range [up-to]
  (cond
    (< up-to 1) ()
    :else (cons (+ up-to -1) (my-range (+ up-to -1))))
  )

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons (seq a-seq) (tails (rest a-seq)))
    )
  )

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq)))
  )

(defn rotations [a-seq]
  (cond
    (empty? a-seq) '(())'
    :else (rest (map concat (tails a-seq) (reverse (inits a-seq)))))
  )

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (let [freqs2 (if (contains? freqs (first a-seq))
                         (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq))))
                         (assoc freqs (first a-seq) 1)
                         )]
            (my-frequencies-helper freqs2 (rest a-seq)))
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) '()
    :else (let [[avain n] (first a-map)]
            (concat (repeat n avain) (un-frequencies (rest a-map)))))
  )

(defn my-take [n coll]
  (cond 
    (or (empty? coll) (< n 1)) '()
    :else (cons (first coll) (my-take (dec n) (rest coll))))
  )

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (< n 1) coll
    :else (my-drop (dec n) (rest coll))
  ))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)]
  )

(defn seq-merge [a-seq b-seq]
  (cond
    (or (empty? a-seq) (empty? b-seq)) (concat a-seq b-seq)
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    )
  )

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    :else
      (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))
    )
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

