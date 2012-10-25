(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [tama (first a-seq)]
    (if (singleton? a-seq)
      tama
      (if (empty? a-seq)
        nil
        (max tama (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2)
  )

(defn longest-sequence [a-seq]
  (let [tama (first a-seq)]
    (if (singleton? a-seq)
      tama
      (if (empty? a-seq)
        nil
        (seq-max tama (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq))))
  )

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  ))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (not (pred? (first a-seq)))
      (cons (first a-seq) (rest a-seq))
      (my-drop-while pred? (rest a-seq))
      )))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (not (= (first a-seq) (first b-seq)))
      false
      (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (not (or (empty? seq-1)
               (empty? seq-2)))
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ()
    ))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()
  ))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    ()
  ))

(defn tails [a-seq]
  (if (not (empty? a-seq))
    (cons a-seq (tails (rest a-seq)))
    '(())
  ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (and (> n 0)
           (> (count coll) 0))
    (cons (first coll) (my-take (dec n) (rest coll)))
  ))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))
  ))

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq)))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

