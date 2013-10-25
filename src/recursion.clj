(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (not (contains? coll (second coll)))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (last coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
         (max-element (rest a-seq))))))

(defn length [seq-x]
  (if (empty? seq-x)
    0
    (+ 1
       (length (rest seq-x)))))

(defn seq-max [seq-1 seq-2]
  (if (> (length seq-1) (length seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (> (length (first a-seq))
           (length (longest-sequence (rest a-seq))))
      (first a-seq)
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
     ()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     ()))

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
  (if
   (or (empty? seq-1) (empty? seq-2))
     ()
   (cons (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (and (* n (power n (dec k))))))

(defn fib [n]
  (if (zero? n)
    0
    (+ (dec n))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ()
    (cons a-seq
          (tails (disj a-seq (first a-seq))))))

(defn inits [a-seq]
  ())

(defn rotations [a-seq]
  ())

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-count (if (= (first a-seq) (rest a-seq))
                     (inc freqs)
                      freqs)]
      (my-frequencies-helper new-count
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
;  (if (empty? coll)
;    ()
;    (let [new-count (inc n)]
;      (if (= new-count n)
;        ()
;        (cons (first coll) (my-take new-count coll))))))
  ())

(defn my-drop [n coll]
  ())

(defn halve [a-seq]
  ())

(defn seq-merge [a-seq b-seq]
  ())

(defn merge-sort [a-seq]
  ())

(defn split-into-monotonics [a-seq]
  ())

(defn permutations [a-set]
  ())

(defn powerset [a-set]
  ())

