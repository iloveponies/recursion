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
  (if (empty? coll)
    nil
   (if(singleton? coll)
    (first coll)
    (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
   (if(singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)(count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
   (if(singleton? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
   (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))
     )))

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
    (and (empty? a-seq) (empty? b-seq))
       true
    (and (= (first a-seq) (first b-seq)) (and (not= nil (first a-seq)) (not= nil (first b-seq))))
       (seq= (rest a-seq) (rest b-seq))
    :else
       false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2))
       ()
       (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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
    :else
     (+(fib (dec n)) (fib (dec (dec n))))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (> 0 how-many-times)
     ()
    (= how-many-times 1)
      (vector what-to-repeat)
    :else
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= 0 up-to)
     (vector )
    (= up-to 1)
      (vector 0)
    :else
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (vector ())
    :else
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq)
      (vector ())
    :else
      (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
  (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [ele (first a-seq)]
    (if(contains? freqs ele)
      (my-frequencies-helper (assoc freqs ele (inc (get freqs ele))) (rest a-seq))
      (my-frequencies-helper (assoc freqs ele 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map)
      ()
    :else
     (let [[ele number] (first a-map)]
      (concat (repeat number ele) (un-frequencies (rest a-map))))))

(defn my-take-helper [n coll]
  [:-])

(defn my-take [n coll]
  (cond
    (empty? coll)
      ()
    (= n 0)
      ()
    :else
      (concat (vector (first coll)) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      ()
    (= n 0)
      coll
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-size (int (/ (count a-seq) 2))]
    [(my-take half-size a-seq) (my-drop half-size a-seq)]))

(defn seq-merge [a-seq b-seq]
   (cond
      (empty? a-seq)
        b-seq
     (empty? b-seq)
        a-seq
      :else
        (let [a (first a-seq),
              b (first b-seq)]
          (if (< a b)
            (concat (vector a) (seq-merge (rest a-seq) b-seq))
            (concat (vector b) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [[one two] (halve a-seq)]
      (seq-merge (merge-sort one) (merge-sort two)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

