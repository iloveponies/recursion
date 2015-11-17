(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) 
    nil
    (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) 
    nil
    (if (singleton? a-seq) 
      (first a-seq) 
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) 
      (cons (first a-seq) (my-filter pred? (rest a-seq))) 
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq)) 
      (if (< 0 (count (my-filter pred? (rest a-seq)))) 
        (cons (first a-seq) (my-take-while pred? (rest a-seq))) 
        '()) 
      (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq) (if (empty? b-seq) true false)
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (== k 0) 1
    (== k 1) n
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    (== n 2) 1
    (== n 3) 2
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) '()
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (== 0 up-to) '()
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (vector a-seq)
    :else (cons a-seq  (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq) 
    (vector a-seq)
    (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (if (contains? freqs (first a-seq)) 
      (my-frequencies-helper 
        (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) 
        (rest a-seq)) 
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) a-map
    :else (apply concat (map (fn [x] (repeat (val x) (first x))) a-map))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (== 0 n) '()
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (== 0 n) coll
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (apply vector (cons 
    (my-take (int (/ (count a-seq) 2)) a-seq)
    (vector (my-drop (int (/ (count a-seq) 2)) a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq)) 
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq)) 
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (== 1 (count a-seq)) a-seq
    :else 
      (seq-merge 
        (merge-sort (my-take (int (/ (count a-seq) 2)) a-seq)) 
        (merge-sort (my-drop (int (/ (count a-seq) 2)) a-seq)))))

(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

