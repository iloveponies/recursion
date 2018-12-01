(ns recursion)

(defn my-sum [coll]
  (if (empty? coll)
    0
    (+ (first coll) (my-sum (rest coll)))))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (= (count seq-1) (count seq-2))
    seq-2
    (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not (= elem (first a-seq)))
      (sequence-contains? elem (rest a-seq))
   :else
      true))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (seq a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (not (= (count a-seq) (count b-seq)))
     false
   (and (empty? a-seq) (empty? b-seq))
     true
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (and (empty? seq-2) (empty? seq-2))
     ()
     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
   (zero? n)
     0
   (zero? k)
     1
   :else
     (* n (power n (dec k)))))




(defn fib [n]
  (cond
   (= n 0)
     0
   (= n 1)
     1
   :else
    (+      (fib (- n 1))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (= how-many-times -1)
     ()
   (= how-many-times 0)
     ""
   :else
     (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= up-to 0)
     ()
   :else
     (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ());list adds the last empty '()' in the result
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))


(defn my-frequencies-helper [freqs a-seq]
   (if (empty? a-seq)
    freqs
    (let [new-count (if (contains? freqs (first a-seq))
                      (inc (get freqs (first a-seq)))
                      1)]
      (my-frequencies-helper (assoc freqs (first a-seq) new-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    '{}
    (my-frequencies-helper {} a-seq)))

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

