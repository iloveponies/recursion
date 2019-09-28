(ns recursion)

(defn product [coll]
  (if (empty? coll)
  1
  (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)
  ))

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
    (max (first a-seq) (max-element (rest a-seq))
    ))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
  (if (singleton? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (true? (pred? (first a-seq)))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]

  (if(empty? a-seq)
    false
   (if (= (first a-seq) elem)
    true
    (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   (rest a-seq)
   (pred? (first a-seq))
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   ()))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   (rest a-seq)
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   (seq a-seq)
   ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
   true
   (or (empty? a-seq) (empty? b-seq))
   false
   (= (first a-seq) (first b-seq))
   (seq= (rest a-seq) (rest b-seq))
   :else
   false
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2))
   '()
   :else
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   ))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (= 0 n)
   0
   (= 1 n)
   1
   :else
   (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0)
   '()
   :else
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (<= up-to 0)
   '()
   :else
   (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (cond
   (empty? a-seq)
   '(())
   :else
   (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq)
   '(())
   :else
   (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))
   ))
(defn rotatate-helper [a-seq number-of-iterations]
  (if (empty? a-seq)
    '(())
   (if(= 0 number-of-iterations)
    '()
    (cons (seq a-seq) (rotatate-helper (cons (first (reverse a-seq)) (reverse (rest(reverse a-seq)))) (dec number-of-iterations)))
    )))

(defn rotations [a-seq]
  (rotatate-helper a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq)
   freqs

   (empty? freqs)
   (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))

   (contains? freqs (first a-seq))
   (my-frequencies-helper  (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))

  :else
   (my-frequencies-helper  (assoc freqs (first a-seq) 1) (rest a-seq))
   ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
(cond
 (empty? a-map)
 '()
 :else
 (concat (repeat (first (rest(first a-map))) (first (first a-map)) ) (un-frequencies (rest a-map)))
 )
)

(defn my-take [n coll]
  (cond

   (or (= n 0) (empty? coll))
   '()
   :else
   (cons (first coll) (my-take (dec n) (rest coll)))
   ))



(defn my-drop [n coll]
  (cond
   (= 0 n)
   coll
   :else
   (my-drop (dec n) (rest coll))
   ))


(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)
   ))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
   b-seq

   (empty? b-seq)
   a-seq

   (< (first a-seq) (first b-seq))
   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))

   :else
   (cons (first b-seq) (seq-merge a-seq (rest b-seq))
   )))


(defn merge-sort [a-seq]
  (cond
   (empty? a-seq)
   '()
   (= (count a-seq) 1)
   a-seq

   :else
   (let[[left right] (halve a-seq)]
    (seq-merge (merge-sort left) (merge-sort right))
     )

   ))

(defn monotonic-prefix [sequence]
  (let [asc (map <= sequence (next sequence))
      desc (map >= sequence (next sequence))
      asc-true (take-while #(= true %) asc)
      desc-true (take-while #(= true %) desc)
      asc-count (count asc-true)
      desc-count (count desc-true)]
      (if (> asc-count desc-count)
        (take (inc asc-count) sequence)
        (take (inc desc-count) sequence))))

(defn split-into-monotonics [sequence]
  (when-not (empty? sequence)
    (let [mons (monotonic-prefix sequence)]
      (cons mons (split-into-monotonics (drop (count mons) sequence))))))


(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

