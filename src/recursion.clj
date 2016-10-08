(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    (first a-seq)
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (longest-sequence (conj
                        (subvec a-seq 2)
                        (seq-max (first a-seq) (second a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
        (first a-seq)
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
  (if (and
        (not-empty a-seq)
        (pred? (first a-seq)))
    (cons
      (first a-seq)
      (my-take-while pred? (rest a-seq)))
    ()
    ))

(defn my-drop-while [pred? a-seq]
  (if
    (and
      (not-empty a-seq)
      (pred? (first a-seq)))
    (my-drop-while pred? (rest a-seq))
    (lazy-seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else
      (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (and (not-empty seq-1) (not-empty seq-2))
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))
    (lazy-seq seq-1 seq-2)))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+
      (fib (- n 1))
      (fib (- n 2))
      )))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (= (count a-seq) 0)
    '(())
    (cons (lazy-seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    (into {} (reverse freqs))
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs
                               (first a-seq)
                               (inc (get freqs (first a-seq))))
                             (rest a-seq))

      (my-frequencies-helper (assoc freqs
                               (first a-seq) 1)
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat
      (repeat (first (vals a-map)) (first (keys a-map)))
      (un-frequencies (rest a-map))
      )))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (not (nth coll n false))
    ()
    (cons (nth coll n) (my-drop (inc n) coll))))

(defn halve [a-seq]
  (letfn [(count-half [a-seq] (int (/ (count a-seq) 2)))]
  (vector
    (my-take (count-half a-seq) a-seq)
    (my-drop (count-half a-seq) a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
    (>= (first a-seq) (first b-seq)) (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

