(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))) )

(defn singleton? [coll]
  (and (not (nil? (first coll))) (empty? (rest coll))) )

(defn e-or-s? [coll]
  (or (empty? coll)(singleton? coll)))

(defn my-last [coll]
  (if (e-or-s? coll)
    (first coll)
    (my-last (rest coll))) )

(defn max-element [a-seq]
  (if (e-or-s? a-seq)
    (first a-seq)
    (max (first a-seq)(max-element (rest a-seq)))) )

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1)(count seq-2))
    seq-2
    seq-1) )

(defn longest-sequence [a-seq]
  (if (e-or-s? a-seq)
    (first a-seq)
    (seq-max (first a-seq)(longest-sequence (rest a-seq)))) )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))) )

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq)) ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)(my-take-while pred? (rest a-seq)))
   :else '() ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq) ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)(empty? b-seq)) true
   (or (empty? a-seq)(empty? b-seq)) false
   (= (first a-seq)(first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false ))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1)(empty? seq-2))
    '()
    (cons (f (first seq-1)(first seq-2)) (my-map f (rest seq-1)(rest seq-2)))
   ))

(defn power [n k]
  (if
    (= k 0)
    1
    (* n (power n (dec k)))) )

(defn fib [n]
  (if
    (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)) )))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)) ))

(defn my-range [up-to]
  (if
    (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to))) ))

(defn tails [a-seq]
  (if
    (empty? a-seq)
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq))) ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (partition (count a-seq) 1 (concat (rest a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [fir (first a-seq)]
    (cond
     (empty? a-seq) freqs
     (contains? freqs fir) (my-frequencies-helper (assoc freqs fir (inc (get freqs fir))) (rest a-seq))
     :else (my-frequencies-helper (assoc freqs fir 1) (rest a-seq)))) )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [values a-map]
  (let [f-key (get (first a-map) 0)
        f-value (get (first a-map) 1)]
    (if
      (empty? a-map)
      values
      (un-frequencies-helper (concat values (repeat f-value f-key)) (rest a-map))
   )))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if
    (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))
   ))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (< n 1) (lazy-seq coll)
   :else (my-drop (dec n) (rest coll)) ))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)(my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq))) ))

(defn merge-sort [a-seq]
  (let [h-1 (get (halve a-seq) 0)
        h-2 (get (halve a-seq) 1)]
    (if
      (< (count a-seq) 2)
      (lazy-seq a-seq)
      (seq-merge (merge-sort h-1) (merge-sort h-2))
   )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

