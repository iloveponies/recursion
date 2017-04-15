(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq) (if (empty? (rest a-seq))
                         (first a-seq)
                         (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
  seq-1
  seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (if (empty? (rest a-seq))
                             (first a-seq)
                             (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not= elem (first a-seq))
     (sequence-contains? elem (rest a-seq))
   :else
     true))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (cond
     (pred? (first a-seq))
       (cons (first a-seq)
             (my-take-while pred? (rest a-seq)))
      :else ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (cond
     (pred? (first a-seq))
       (my-drop-while pred? (rest a-seq))
     :else
       a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and
     (= (first b-seq) (first a-seq))
     (not (or (empty? a-seq) (empty? b-seq))))
       (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+
       (fib (- n 1))
       (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons () a-seq)
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
 (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
   nil
   nil))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    ()
    (cons
     (first coll)
     (my-take (dec n)
               (rest coll)))))

(defn my-drop [n coll]
  (if (> n (count coll))
    ()
    (reverse (my-take n (reverse coll)))))

(defn halve [a-seq]
  (let [timesaver (count a-seq)]
   (let [first-half (int (/ timesaver 2))]
     (let [second-half (if (= timesaver 1)
                         1
                         (- timesaver first-half))]
    (vector (my-take first-half a-seq) (my-drop second-half a-seq))))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
     (if (empty? b-seq)
       ()
       (cons (first b-seq) (seq-merge a-seq (rest b-seq))))
     (if (empty? b-seq)
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
       (if (> (first a-seq) (first b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [piece (halve a-seq)]
     (seq-merge
      (merge-sort (get piece 0))
      (merge-sort (get piece 1))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

