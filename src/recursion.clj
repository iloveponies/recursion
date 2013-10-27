(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll))) (nil? (first (rest coll)))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll) 
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq) 
      (first a-seq) 
      (max-element (rest a-seq)))))

(defn first-longer-seq? [s1 s2]
  (cond (empty? s1) false
        (empty? s2) true
        (first-longer-seq? (rest s1) (rest s2)) true
        :else false))

(defn seq-max [seq-1 seq-2]
  (if (first-longer-seq? seq-1 seq-2)
    seq-1
    seq-2))
  

(defn longest-sequence [a-seq]  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (longest-sequence(rest a-seq)) (first a-seq))
          ))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) 
    false
    (if (= elem (first a-seq)) 
      true 
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
  '()
  (if (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) 
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (and (empty? a-seq) (empty? b-seq)) 
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false
      )))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (f (first seq-2)))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (= 0 n) 
    0
    (if (= 1 n) 
      1
     (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times) 
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()
    ))

(defn my-range [up-to]
  (if (== 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  [:-])


(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq) r (rest a-seq)] 
    (cond
      (empty? a-seq) freqs
      (nil? (get freqs f)) (my-frequencies-helper (assoc freqs f 1) r)
      :else (my-frequencies-helper (assoc freqs f (inc (get freqs f))) r))))



(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll)) 
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll)) 
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq) (my-drop mid a-seq)]))

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

