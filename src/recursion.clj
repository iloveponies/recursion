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
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq) 
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq1-longer? [seq-1 seq-2]
  (cond
   (empty? seq-1) false
   (empty? seq-2) true
   :else (seq1-longer? (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (if (seq1-longer? seq-1 seq-2)
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond 
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
   (empty? a-seq) []
   (not (pred? (first a-seq))) []
   :else (cons (first a-seq)
               (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map [f seq-1 seq-2]
   (if (or (empty? seq-1) (empty? seq-2))
     []
     (cons
      (f (first seq-1) (first seq-2)) 
	  (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
   (< how-many-times 1) '()
   (== how-many-times 1) (cons what-to-repeat nil)
   :else (cons what-to-repeat
              (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [minus-one (dec up-to)]
   (cond
    (< minus-one 0) '()
    (== 0 minus-one) (cons 0 nil)
    :else (cons minus-one (my-range minus-one)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() nil)
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (recur-inits (reverse a-seq)))

(defn recur-inits [a-seq]
  (if (empty? a-seq)
    (cons '() nil)
    (cons 
     (reverse (cons (first a-seq) (rest a-seq))) 
     (reverse (reinits (rest a-seq))))))

(defn rotations-helper [a-seq n length]
  (if (== n length)
    '()
    (concat a-seq (rotations-helper (concat (rest a-seq) (vector (first a-seq))) (inc n) length))))

(defn rotations-helper [a-seq n length]
  (if (== n length)
    '()
    (concat a-seq (rotations-helper (concat (rest a-seq) (vector (first a-seq))) (inc n) length))))

(defn rotations [a-seq]
	(rotations-helper a-seq 0 (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take-helper [n coll counter]
  (if (or (== n counter) (empty? coll))
    '()
    (cons (first coll) (my-take-helper n (rest coll) (inc counter)))))

(defn my-take [n coll]
  (my-take-helper n coll 0))

(defn my-drop-helper [n coll counter]
  (if (or (== n counter) (empty? coll))
    coll
    (my-drop-helper n (rest coll) (inc counter))))

(defn my-drop [n coll]
  (my-drop-helper n coll 0))

(defn halve [a-seq]
  (let [halfway (int (/ (count a-seq) 2))]
    (vector (my-take halfway a-seq) (my-drop halfway a-seq))))

(defn smaller-seq [a-seq b-seq]
  (if (< (first a-seq) (first b-seq)) a-seq b-seq))

(defn seq-merge [a-seq b-seq]
  (cond 
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [half (halve a-seq)
          first-half (get half 0)
          second-half (get half 1)]
    (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])