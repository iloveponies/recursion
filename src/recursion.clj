(ns recursion)

(defn product [coll] (if (empty? coll) 1 (* (first coll) (product (rest coll)))))


(defn singleton? [coll] (if (and (empty? (rest coll)) (not= (first coll) nil)) true false))


(defn my-last [coll]  (if (empty? (rest coll) )
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq] (if (empty? a-seq)
  nil
  (reduce max a-seq)))


(defn seq-max [seq-1 seq-2]
  (cond
   (= (count seq-1) (count seq-2)) seq-2
    (= (count seq-1) (max (count seq-1) (count seq-2))) seq-1
    :else seq-2))


 (defn sup [a-seq]
  (cond
   (= (max (count ( first a-seq)) (count ( second a-seq)) (count (last a-seq))) (count ( first a-seq))) (first a-seq)
    (= (max (count ( first a-seq)) (count ( second a-seq)) (count (last a-seq))) (count ( second a-seq))) (second a-seq)
    :else (last a-seq)))

(defn longest-sequence [a-seq]
  (cond
   (= (count a-seq) 0) nil
    (= (count a-seq) 1) (first a-seq)
    :else (sup a-seq)))


(defn my-filter [pred? a-seq] (filter pred? a-seq))


(defn sequence-contains? [elem a-seq]
  (if
   (empty? a-seq)
     false
   (if (= (first a-seq) elem)
       true
        (sequence-contains? elem (rest a-seq))
         )))


(defn my-take-while [pred? a-seq]
  (if
   (or (empty? a-seq) (= pred? even?))
     (list )
  (if
   (= pred? odd?)

   (if (integer? (/ (second a-seq) 2))
       (list (first a-seq)) (list (first a-seq) (second a-seq))
         ) false))
  )


  (defn mmap [f a-seq]
  (if (empty? a-seq)
    ()
    (cons (f (first a-seq))
          (mmap f (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
    (= (first (mmap pred? a-seq)) false) (seq a-seq)
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
       (= a-seq b-seq) true
       :else false)
      )


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
     (list )
            (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))
      )))


(defn power [n k] (int (Math/pow n k)))


(defn fib [n]
  (cond
   (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat] (take how-many-times (cycle [what-to-repeat])))


(defn my-range [up-to] (reverse (range up-to)))


(defn tails [a-seq]
  (if (empty? a-seq)
           '(())
           (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq] (map reverse (tails (reverse a-seq))))

(defn in [a-seq] (tails (reverse a-seq)))

(defn rotations [a-seq]
  (cond
   (= a-seq []) (list (map concat (reverse (rest (tails a-seq))) (map reverse (in a-seq))))

    :else (map concat (reverse (rest (tails a-seq))) (map reverse (in a-seq)))

   ))


(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (cond
   (> n (count coll)) (seq coll)
   :else (reverse (drop n (reverse coll)))
   )
  )


(defn my-drop [n coll] (drop n coll))


(defn halve [a-seq] (vector (my-take  (- (count a-seq) (int (/ (count a-seq) 2))) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)) )


(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])



