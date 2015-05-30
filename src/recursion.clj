(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and
       (empty? (rest coll))
       (not (empty? coll)))
    true
    false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max
       (first a-seq)
       (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (>
       (count seq-1)
       (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max
       (first a-seq)
       (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons
       (first a-seq) (my-filter pred? (rest a-seq)))
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
     '()
  (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
    '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   '()
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? (rest a-seq)) (empty? (rest b-seq)))
    true
    (and
     (= (first a-seq) (first b-seq))
     (= (count a-seq) (count b-seq)))
    (seq= (rest a-seq) (rest b-seq))
    :else
    false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or
    (empty? seq-1)
    (empty? seq-2))
   '()
   :else
   (cons
    (f
     (first seq-1)
     (first seq-2))
    (my-map
     f
     (rest seq-1)
     (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0)
   0
   (= n 1)
   1
   :else (+
          (fib (dec n))
          (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0)
    '()
    :else (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (<= up-to 0)
   '()
   :else
   (cons
    (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq)
   '(())
   :else
   (cons a-seq
         (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond (empty? a-seq)
        '(())
  :else
    (rest
     (map
      concat
      (tails a-seq)
      (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]

  (cond
   (empty? a-seq)
   freqs
   (not (nil? (freqs (first a-seq))))
        (my-frequencies-helper
         (conj freqs
               {(first a-seq) (inc (freqs (first a-seq)))})
         (rest a-seq))
   :else (my-frequencies-helper
          (conj freqs {(first a-seq) 1}) (rest a-seq))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies-helper [build a-map]
  (cond
   (empty? a-map)
   build
   :else (un-frequencies-helper
          (concat build
                  (repeat
                   (second (first a-map))
                   (first (first a-map))))
          (rest a-map))))


(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (cond
   (or (== n 0) (empty? coll))
   '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (or (== n 0) (empty? coll))
   coll
   :else (my-drop (dec n ) (rest coll))))

(defn halve [a-seq]
  [(my-take
    (int
     (/
      (count a-seq)
      2)) a-seq)
   (my-drop
    (int
     (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
   b-seq
   (empty? b-seq)
   a-seq
   (< (first a-seq) (first b-seq))
   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons
          (first b-seq)
          (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (<= (count a-seq) 2)
   (seq-merge (first (halve a-seq)) (second (halve a-seq)))
   :else (seq-merge
          (merge-sort (first (halve a-seq)))
          (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

