(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (empty? (rest coll)) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   (empty? (rest (rest a-seq))) (max (first a-seq) (second a-seq))
   :else (max-element (cons (max (first a-seq) (second a-seq)) (rest (rest a-seq))))))

(defn seq-max [seq-1 seq-2])

(defn longest-sequence [a-seq])

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq))) [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq)) a-seq))) 

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== 0 k) 1
   (== 1 k) n
   :else (* (* n (power n (- k 1))))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) [] (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (== 0 up-to) [] (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll)) [] (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (== 0 n) coll (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        start (my-take half a-seq)
        end (my-drop half a-seq)]
    (vec(cons start end))))

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