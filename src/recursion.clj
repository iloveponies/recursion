(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
    (if (or (empty? coll) (not (empty? (rest coll))))
      false
      true))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [rest-max (max-element (rest a-seq))
          first-el (first a-seq)]
      (if rest-max
        (max first-el rest-max)
        first-el))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [rest-max (longest-sequence (rest a-seq))
          first-seq (first a-seq)]
      (if rest-max
        (seq-max first-seq rest-max)
        first-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [rest-seq (my-filter pred? (rest a-seq))
          first-seq (first a-seq)]
      (if (pred? first-seq)
        (cons first-seq rest-seq)
        rest-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [first-seq (first a-seq)]
    (if (or (empty? a-seq) (not (pred? first-seq)))
      '()
      (cons first-seq (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (let [new-up-to (dec up-to)]
      (cons new-up-to (my-range new-up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [seq-tails (tails a-seq)
          seq-inits (inits a-seq)]
      (rest (map concat seq-tails seq-inits)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-seq (first a-seq)
          new-freqs (my-frequencies-helper freqs (rest a-seq))]
      (assoc new-freqs
             first-seq
             (inc (get new-freqs first-seq 0))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[map-key map-value] (first a-map)]
      (concat (repeat map-value map-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (zero? n)
      (seq coll)
    (> n (count coll))
      '()
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector (my-take mid a-seq) (my-drop mid a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [[left-half right-half] (halve a-seq)]
      (seq-merge
        (merge-sort left-half)
        (merge-sort right-half)))))

; it's a try but seems a bit complicated
(defn split-into-monotonics-helper [n a-seq]
  (if (empty? a-seq)
    '()
    (cons
      (take n a-seq)
      (split-into-monotonics-helper n (drop n a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [seq-inits (inits a-seq)
          best-split-index (int (/ (count seq-inits) 2))
          best-split-count (count (first (drop
                                            best-split-index
                                            seq-inits)))]
      (split-into-monotonics-helper best-split-count a-seq))))


(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

