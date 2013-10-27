(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll))))
)

(defn singleton? [coll]
  (if (empty? coll) false
    (if (empty? (rest coll)) true
      false))
)

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll) (my-last (rest coll)))
)

(defn max-element [a-seq]
  (cond (empty? a-seq) nil 
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))



(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2)
  )

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq )
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))) 
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq))))
  )

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq)))
)

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) 
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-take-while pred? (rest a-seq)))
          '())))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) 
            (my-drop-while pred? (rest a-seq))
        :else   a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2))
                    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (= n 0) 0
        (= k 0) 1
        :else (* n (power n (- k 1))))
)

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        (= n 2) 1
        :else (+ (fib (- n 1)) (fib (- n 2))))
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (not (pos? how-many-times)) '()
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to) '()
      (cons (dec up-to) (my-range (dec up-to))))) 

(defn tails [a-seq]
  (if (empty? a-seq) (cons a-seq '())
      (cons a-seq (tails (rest a-seq))))) 

(defn inits [a-seq]
  (let [reveseq (reverse a-seq)]
    (map reverse (tails reveseq))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
  (rest (map concat (tails a-seq)  (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]) 

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (cond (= n 0) '()
        (empty? coll) '()
        :else (cons (first coll) (my-take (- n 1) (rest coll))))) 

(defn my-drop [n coll]
  (cond (= n 0) coll
        (empty? coll) '()
        :else (my-drop (dec n)  (rest coll))))


(defn halve [a-seq])



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

