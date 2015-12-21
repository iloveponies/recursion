(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (or (my-last (rest coll))
        (first coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
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

   (= elem (first a-seq))
     true

    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq

   (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))

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
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k)
     1

   (== 1 k)
     n

   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
     0
   (== n 1)
     1
   :else
     (+ (fib (- n 1))
        (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (rest (map concat
          (tails a-seq)
          (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [h (first a-seq)]
      (my-frequencies-helper
       (if (get freqs h)
         (assoc freqs h (inc (get freqs h)))
         (assoc freqs h 1))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[e n] (first a-map)]
      (concat (my-repeat n e)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    ()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (cond
    (empty? a-seq)
      (list a-seq)

    (singleton? a-seq)
      (vector () a-seq)

    :else
      (let [half (int (/ (count a-seq) 2))]
        (vector (my-take half a-seq)
             (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      ()

    (empty? a-seq)
      (cons (first b-seq)
            (seq-merge a-seq (rest b-seq)))

    (empty? b-seq)
      (cons (first a-seq)
            (seq-merge (rest a-seq) b-seq))

    (< (first a-seq) (first b-seq))
      (cons (first a-seq)
            (seq-merge (rest a-seq) b-seq))

    :else
      (cons (first b-seq)
            (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
  (def rising?
    (fn [s] (or (singleton? s) (>= (first s) (second s)))))

  (def asc-start
    (fn [a-seq]
      (cond
       (empty? a-seq)
       (vector)

       (not (rising? a-seq))
       (vector (first a-seq))

       :else
       (cons (first a-seq)
             (asc-start (rest a-seq))))))

  (def desc-start
    (fn [a-seq]
      (cond
       (empty? a-seq)
       (vector)

       (rising? a-seq)
       (vector (first a-seq))

       :else
       (cons (first a-seq)
             (desc-start (rest a-seq))))))

  (cond
   (empty? a-seq)
   nil

   (rising? a-seq)
   (let [rs (asc-start a-seq)]
     (cons rs
           (split-into-monotonics (my-drop (count rs) a-seq))))

   :else
   (let [ds (desc-start a-seq)]
     (cons ds
           (split-into-monotonics (my-drop (count ds) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

