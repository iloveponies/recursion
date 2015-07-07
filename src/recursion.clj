(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
  (if (singleton? coll)
    (first coll)
    (my-last (rest coll)))))

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
   (not= elem (first a-seq))
       (sequence-contains? elem (rest a-seq))
   :else
       true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
      a-seq
   (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))))

;1 3 4 5  ->  4 5
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
  (if (not (or (empty? seq-1) (empty? seq-2)))
    (cons (f
           (first seq-1)
           (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))
    ()))



(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
    (+ (fib (dec n))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (> how-many-times 0)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
   :else
    ()))

(defn my-range [up-to]
  (cond
   (> up-to 0)
    (cons (dec up-to)
          (my-range (dec up-to)))
   :else
    ()))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons a-seq ())
   :else
    (cons a-seq
         (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq) (cons a-seq ())
   :else
    (cons a-seq
          (inits (reverse (rest (reverse a-seq)))))))

(defn do-rotations [n a-seq]
  (if (= n 0)
    ()
    (cons (seq a-seq)
          (do-rotations (dec n)
                        (concat (rest a-seq)
                                [(first a-seq)])))))

(defn rotations [a-seq]
  (do-rotations (count a-seq) a-seq))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (not (contains? freqs (first a-seq)))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

 (defn un-frequencies-helper [the-seq a-map]
   (if (empty? a-map)
     the-seq
     (un-frequencies-helper (concat (repeat (second (first a-map))
                                            (first (first a-map)))
                                    the-seq)
                            (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    ()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-index (int (/ (count a-seq) 2))
        first-half (my-take half-index a-seq)
        later-half (my-drop half-index a-seq)]
    [first-half later-half]))

(defn seq-merge-helper [the-seq a-seq b-seq]
  (cond
   (empty? a-seq) (concat the-seq b-seq)
   (empty? b-seq) (concat the-seq a-seq)
   (< (first a-seq) (first b-seq)) (seq-merge-helper (concat the-seq [(first a-seq)]) (rest a-seq) b-seq)
   (> (first a-seq) (first b-seq)) (seq-merge-helper (concat the-seq [(first b-seq)]) a-seq (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper () a-seq b-seq))

(defn merge-sort [a-seq]
  (let [halved (halve a-seq)]
    (if (< (count a-seq) 2)
      a-seq
      (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
 [:-])

(defn powerset [a-set]
  [:-])

