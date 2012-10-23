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
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [current (first a-seq)]
    (if (singleton? a-seq)
      current
      (if (empty? a-seq)
        nil
        (max current
             (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (let [seq-size (fn ! [my-seq]
                   (if (empty? my-seq)
                     0
                     (+ 1 (! (rest my-seq)))))]
    (if (> (seq-size seq-1) (seq-size seq-2))
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [current (first a-seq)]
    (if (singleton? a-seq)
      current
      (if (empty? a-seq)
        nil
        (seq-max current
                 (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (let [current (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? current)
        (cons current (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [current (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? current) (cons current (my-take-while pred? (rest a-seq)))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (or
    (and (empty? a-seq) (empty? b-seq))
    (and
      (= (first a-seq)
         (first b-seq))
      (seq= (rest a-seq)
            (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (<= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

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

