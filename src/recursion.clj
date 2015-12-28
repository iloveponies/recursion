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
       (my-last (rest coll)))
    ))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

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
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false                        ; the empty sequence contains only numbers
   (not (= elem (first a-seq)))
     (sequence-contains? elem (rest a-seq)) ; we got a number, let's check the rest
   :else
     true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      (seq a-seq)
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n)
      0
    (= 1 n)
      1
    :else
      (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    '()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (cons () '())
    :else
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [t (tails a-seq)
       my-init (reverse (inits a-seq))]
    (seq (set (my-map concat t my-init)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
           new-freqs (if (contains? freqs elem)
                       (assoc freqs elem (inc (freqs elem)))
                       (assoc freqs elem 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (second (first a-map)) (first (first a-map)) )
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (let [taken (my-take n coll)]
    (if (empty? taken)
      coll
      (my-drop (- n 1) (rest coll)))))

(defn countseq [a-seq known-size]
  (if (empty? a-seq)
    known-size
    (countseq (rest a-seq) (+ 1 known-size))))

(defn halve [a-seq]
  (let [size-seq (countseq a-seq 0)
        half (if (even? size-seq)
               ((/ size-seq 2))
               (/ (- size-seq 1) 2))]
    [ (my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      '()
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
    ))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)
      '()
    (empty? (rest a-seq))
      a-seq
    :else
      (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (rest (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

