(ns recursion)

(defn product [coll]
  (if
      (empty? coll) 1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (= (first coll) nil)) 
   (= (first (rest coll)) nil)))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (empty? (rest coll)) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn longer-seq [seq-1 seq-2]
  (cond
   (empty? seq-1) 2
   (empty? seq-2) 1
   :else (longer-seq (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (let [longer (longer-seq seq-1 seq-2)]
    (case longer
      1 seq-1
      2 seq-2
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(defn my-filter [pred? a-seq]
  (if
      (empty? a-seq) a-seq
      (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else  (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (== up-to 0) '()
   (== up-to 1) '(0)
   :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  [:-])

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
  [:-])

(defn powerset [a-set]
  [:-])

