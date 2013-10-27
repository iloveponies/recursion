(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (first coll)
    (empty? (rest coll))
    false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element(rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
         seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  :-)

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (not (== (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0
   (zero? k) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map sort (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (defn rotations [a-seq]
  (letfn [(rotations-helper [seq-1 seq-2]
            (cond
             (empty? seq-1) '()
             :else (cons (concat seq-1 seq-2)(rotations-helper (rest seq-1)(concat seq-2 (take 1 seq-1))))))]
          (cond
           (empty? a-seq) '(())
           :else (rotations-helper a-seq '()))
    )))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   :else (let [elem (first a-seq)
               elem-count (if (contains? freqs elem) (inc (get freqs elem))1)]
           (my-frequencies-helper (assoc freqs elem elem-count)(rest a-seq)))
   ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) '()
   :else (concat (repeat ((first a-map)1)((first a-map)0))(un-frequencies (rest a-map)))
   ))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-point (int (/ (count a-seq) 2))]
    (cons (my-take half-point a-seq) [(my-drop half-point a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq)(seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq)(seq-merge (rest b-seq) a-seq))
   ))

(defn merge-sort [a-seq]
  (let [halves (halve a-seq)]
    (cond
     (<= (count (halves 1))1) (seq-merge (halves 1)(halves 0))
     :else (seq-merge (merge-sort (halves 0))(merge-sort (halves 1)))
   )))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (loop [a-seq a-seq
         mono '()
         monotonics '()]
    (if (empty? a-seq)
      (if (empty? mono)
        (reverse monotonics)
        (reverse (cons (reverse mono) monotonics)))
      (let [new-mono (cons (first a-seq) mono)]
        (if (or (singleton? new-mono) (monotonic? new-mono))
          (recur (rest a-seq) new-mono monotonics)
          (recur (rest a-seq) [(first a-seq)] (cons (reverse mono) monotonics)))))
    ))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

