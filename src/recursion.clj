(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    (not (singleton? coll)) (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (max (first a-seq)
              (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (and (empty? (rest a-seq))
         (pred? (first a-seq))) (conj () (first a-seq))
    (and (pred? (first a-seq))
         (not (pred? (first (rest a-seq)))))
           (conj () (first a-seq))
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq)
                (concat (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (== (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    (or (empty? (rest seq-1))
        (empty? (rest seq-2)))
          (conj () (f (first seq-1) (first seq-2)))
    :else (cons (f (first seq-1) (first seq-2))
                (concat (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 2)) (fib (dec n)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times )
    ()
    (if (== 1 how-many-times)
      (conj () what-to-repeat)
      (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) ()
    (== 1 up-to) '(0)
    :else (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (seq (vector ()))
    (empty? (rest a-seq))
      (cons (seq a-seq)
            (vector (rest a-seq)))
    :else (cons (seq a-seq)
                (tails (rest a-seq)))))

(defn first-rest [a-seq]
  (cond
    (empty? a-seq) []
    (empty? (rest a-seq)) []
    :else (cons (first a-seq) (first-rest (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq) (seq (vector ()))
    (empty? (rest a-seq))
      (cons (seq a-seq)
            (vector (rest a-seq)))
    :else (cons (seq a-seq) (inits (first-rest a-seq)))))

(defn rotate [a-seq n]
  (cond
    (< 1 n) (cons (seq a-seq) (rotate (concat (rest a-seq) (vector (first a-seq))) (dec n)))
    (== 1 n) (seq (vector a-seq))
    (== 0 n)(seq (vector ()))))

(defn rotations [a-seq]
  (let [n (count a-seq)]
  (rotate a-seq n)))

(defn my-frequencies-helper [freqs a-seq]
  (let [f
         (fn [freqss aseq]
           (if (contains? freqss (first aseq))
             (+ (get freqss (first aseq)) 1)
             1))]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs (first a-seq) (f freqs a-seq))
      (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
   [:-])

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    '()
    (seq (vector (my-take (int (/ (count a-seq) 2)) a-seq)
                 (my-drop (int (/ (count a-seq) 2)) a-seq)))))

(defn remove-min-helper [a a-seq]
  (if (== a (first a-seq))
    (rest a-seq)
    (vector (first a-seq) (remove-min-helper a (rest a-seq)))))

(defn remove-min [a-seq]
  (remove-min-helper (apply min a-seq) a-seq))

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

