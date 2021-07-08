(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not (empty? coll))) true false))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (longest-sequence
       (cons (seq-max (first a-seq) (second a-seq))
             (rest (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) a-seq
      (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) a-seq
      (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
   (or (and (not (empty? a-seq)) (empty? b-seq)) (and (empty? a-seq) (not(empty? b-seq)))) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons ( f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
 (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '( () )
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [head tail]
  (if (empty? tail) '()
    (cons
     (concat tail head)
     (rotate (concat head [(first tail)]) (rest tail)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '( () )
    (rotate [] a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (if (not (contains? freqs (first a-seq)))
      (my-frequencies-helper (assoc freqs (first a-seq) 0) a-seq)
     (my-frequencies-helper
      (assoc freqs (first a-seq) (inc (get freqs (first a-seq)) ))
      (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (let [value (first a-map)]
    (concat (repeat (second value) (first value)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1)) '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (< n 1)) coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [len (int (/ (count a-seq) 2))]
    [ (my-take len a-seq) (my-drop len a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
   (cons (first a-seq)
         (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq)
                (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq)) a-seq
    (apply seq-merge
           (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
   [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

