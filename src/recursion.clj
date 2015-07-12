(ns recursion)

(defn product [coll]
  (apply * coll))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false
    ))

(defn my-last [coll]
   (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (filter pred? a-seq))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (not= elem (first a-seq))
           (sequence-contains? elem (rest a-seq))
        :else true))

(defn my-take-while [pred? a-seq]
    (if (or (empty? a-seq)
        (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
   (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
 (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else
    (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)) )))

(defn power [n k]
  (cond (== n 0) 0
        (== k 0) 1
      :else
        (* n (power n (- k 1)))))

(defn fib [n]
  (cond (= n 0) 0
    (< n 2) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2))) ))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (<= how-many-times 0) ()
        :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond (= up-to 0) ()
        :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond (empty? a-seq) '(())
      :else
        (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [rev (tails (reverse a-seq))]
    (map reverse rev)))

(defn rotations [a-seq]
 (if (empty? a-seq)
    '(())
    (let [t (tails a-seq)
          i (reverse (inits a-seq))]
      (rest (map concat t i)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
        freqs
      (let [head (first a-seq)
            new-freq (assoc freqs head (inc (freqs head 0)))]
            (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
     a-map
     (let [[elem, number] (first a-map)]
          (concat (repeat number elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
      ()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (< (count coll) n)
      ()
  (reverse (my-take n (reverse coll)))))

(defn halve [a-seq]
  (let [total (count a-seq)
        f-numbers (quot total 2)
        f-quater (my-take f-numbers a-seq)
        l-quater (my-drop (- total f-numbers) a-seq)]
    (vector f-quater l-quater)))

(defn seq-merge [a-seq b-seq]
  (cond (empty? b-seq) a-seq
      (empty? a-seq) b-seq
      (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
   (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [[left right] (halve a-seq)
          lsort (merge-sort left)
          rsort (merge-sort right)]
      (seq-merge lsort rsort))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

