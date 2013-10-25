(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
  (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (not (empty? (rest coll))) false
   :else true))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (empty? (rest coll)) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
    (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)(count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   (not (pred? (first a-seq))) (my-filter pred? (rest a-seq))
   :else '()))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)(empty? b-seq)) true
   (or (and (empty? a-seq)(not (empty? b-seq)))
       (and (not (empty? a-seq))(empty? b-seq))) false
   (== (first a-seq)(first b-seq)) (seq= (rest a-seq)(rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)(empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
  (* n (power n (dec k)))))

(defn fib [n]
  (if (= n 0) 0
    (if (= n 1) 1
  (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails(reverse a-seq))))

(defn rotations-helper [a-seq tmp-seq]
  (if (empty? a-seq) a-seq
    (let [new-seq-1 (rest a-seq)
          new-seq-2 (cons (first a-seq) tmp-seq)]
      (cons (concat new-seq-1 (reverse new-seq-2)) (rotations-helper new-seq-1 new-seq-2)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (rotations-helper a-seq []))
  )

(rotations [])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  )

(defn powerset [a-set]
  [:-])

