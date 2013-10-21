(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (+ 1 (my-count (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (my-count seq-1) (my-count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1)
     seq-1
   (empty? seq-2)
     seq-2
   :else
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0)
    0
   (== n 1)
    1
   :else
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [(vector)]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [head tail]
  (let [new-head (rest head)
        new-tail (concat tail (vector (first head)))]
    (if (empty? head)
      (vector)
      (cons (concat new-head new-tail)
            (rotations-helper new-head new-tail)))))

;[2 3 4] [1]
;[3 4] [1 2]
;[4] [1 2 3]

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (vector))))

(defn my-frequencies-helper [freqs a-seq]
  (let [freq (fn [a] (if (get freqs a)
                         (inc (get freqs a))
                         1))]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (assoc freqs (first a-seq) (freq (first a-seq)))
     (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (concat (repeat (val (first a-map)) (key (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    []
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [count-seq (count a-seq)
        split-point (int (/ count-seq 2))]
    [(my-take split-point a-seq)
     (my-drop split-point a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     []
   (empty? b-seq)
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (or (empty? a-seq)
       (> (first a-seq) (first b-seq)))
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   :else
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
