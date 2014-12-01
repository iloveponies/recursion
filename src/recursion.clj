(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
      (* (first coll) (product (rest coll)))))
;; exercise 2
;; TRACE t3295: (recursion/product [1 2 3])
;; TRACE t3296: | (recursion/product (2 3))
;; TRACE t3297: | | (recursion/product (3))
;; TRACE t3298: | | | (recursion/product ())
;; TRACE t3298: | | | => 1
;; TRACE t3297: | | => 3
;; TRACE t3296: | => 6
;; TRACE t3295: => 6

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
      (let [first-val (first a-seq)
            rest-seq (my-filter pred? (rest a-seq))]
        (if (pred? first-val) (cons first-val rest-seq) rest-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq)))) '()
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq)) 
    (and (empty? a-seq) (empty? b-seq))
    (and  (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) 
    n 
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) 
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reversed-seq (reverse a-seq)
        rest-seq (reverse (rest reversed-seq))]
    (if (empty? a-seq) 
      (cons '() '())
      (cons a-seq (inits rest-seq)))))

(defn rotations [a-seq]
  (let [tailseqs (tails a-seq)
        initseqs (reverse (inits a-seq))]
    (set (my-map concat tailseqs initseqs))))

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

