(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll)))
       (nil? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    a-seq
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (seq a-seq)
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))
    '()))

(defn sequence-contains? [elem a-seq]
  (if (seq a-seq)
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))
    false))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) '()
   :else (cons (first a-seq) (my-drop-while pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   ( = (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? a-seq) (empty? b-seq)) '()
   (cons (f (first a-seq) (first b-seq)) (my-map (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (if ( = k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (map (fn [x] what-to-repeat) (range how-many-times)))

(defn my-range [up-to]
  (if ( =  up-to 0) '()
      (cons up-to (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse (seq a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
      (map concat (tails a-seq) (reverse (rest (inits a-seq))) )))

(defn my-frequencies-helper [freqs a-seq]
  (let [elem (first a-seq)]
      (if (empty? a-seq)
     freqs
     (my-frequencies-helper
      (assoc freqs elem (inc (or (get freqs elem) 0)))
      (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (if ( = 1 (last (first a-map)))
      (cons (first (first a-map))  (un-frequencies (dissoc a-map (first (first a-map)))))
      (cons (first (first a-map))  (un-frequencies (update-in a-map
                                                              [(first (first a-map))]
                                                              dec))))))

(defn my-take [n coll]
  (if ( = n 0) '()
      (cons  (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if ( =  n 0) coll
      (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [long (int (/ (count a-seq) 2))]
    (vector (my-take long a-seq) (my-drop long a-seq))))

(defn seq-merge [[f1 & r1] [f2 & r2]]
  (cond
   (nil? f1) (cons f2 r2)
   (nil? f2) (cons f1 r1)
   (< f1 f2) (cons f1 (seq-merge r1 (cons f2 r2)))
   :else (cons f2 (seq-merge r2 (cons f1 r1)))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (apply seq-merge
           (map merge-sort (halve a-seq)))
    a-seq))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
