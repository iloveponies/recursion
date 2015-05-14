(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (conj (my-take-while pred? (rest a-seq)) (first a-seq))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else  a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false ))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (< n 2) n
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (conj (my-range (- up-to 1)) (- up-to 1))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (lazy-seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (lazy-seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [n a-seq]
  (let [new-seq (concat (rest a-seq) [(first a-seq)])]
    (if (= 0 n)
      '()
      (cons new-seq (rotations-helper (dec n) new-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [func (update-in freqs [(first a-seq)] (fnil inc 0))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper func (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [key (first (first a-map))
        val (second (first a-map))]
    (if (empty? a-map)
      '()
      (concat (repeat val key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (nil? (first coll)))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (= 0 n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))
        first-half (my-take middle a-seq)
        second-half (my-drop middle a-seq)]
    (vector first-half second-half)))

(defn seq-merge [a-seq b-seq]
  (let [fa (first a-seq)
        fb (first b-seq)
        smaller (if (or (nil? fb) (and fa (< fa fb)))
                 fa
                 fb)
        new-a-seq (if (= smaller fa)
                (rest a-seq)
                a-seq)
        new-b-seq (if (= smaller fb)
                (rest b-seq)
                b-seq)]
    (if (nil? smaller)
      '()
      (cons smaller (seq-merge new-a-seq new-b-seq)))))

(defn merge-sort [a-seq]
  (let [half (halve a-seq)]
    (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort (first half)) (merge-sort (second half))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

