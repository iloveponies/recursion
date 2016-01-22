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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq)        nil
   (empty? (rest a-seq)) (first a-seq)
   :else                 (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq)        a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)         false
   (= elem (first a-seq)) true
   :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)        ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else                 ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)        ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else                 (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq))  false
   (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
   :else                               false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== n 1)  1
   :else     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [re-reverse (reverse (rest (reverse a-seq)))]
    (if (empty? a-seq)
      (cons () ())
      (cons (seq a-seq) (inits re-reverse)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (distinct (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat
     (repeat (val (first a-map)) (key (first a-map)))
     (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons
     (first coll)
     (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (zero? n)     (seq coll)
   (empty? coll) ()
   :else         (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) (seq b-seq)
   (empty? b-seq) (seq a-seq)
   (<= (first a-seq) (first b-seq)) (cons
                                     (first a-seq)
                                     (seq-merge (rest a-seq) b-seq))
   :else                            (cons
                                     (first b-seq)
                                     (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (== (count a-seq) 1)  (seq a-seq)
   (< (count a-seq) 1) ()
   :else                (let [divided-seq (halve a-seq)]
                          (seq-merge
                           (merge-sort (first divided-seq))
                           (merge-sort (second divided-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

