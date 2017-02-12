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
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq)
               (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
    (if (empty? a-seq) ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
   (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
   (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (and (= (first a-seq) (first b-seq))
               (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
    (if (== k 0) 1
    (* n (power n (dec k)))))

(defn fib [n]
      (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
      (if (<= how-many-times 0) '()
        (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
      (if (<= up-to 0) '()
        (cons (dec up-to)
              (my-range (dec up-to)))))

(defn tails [a-seq]
      (if (empty? a-seq) '(())
        (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
      (let [r (reverse a-seq)]
           (reverse (map reverse (tails r)))))

(defn rotations [a-seq]
      (if (empty? a-seq) '(())
        (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
      (if (empty? a-seq) freqs
        (let [elem (first a-seq)
              sub-freqs (if (contains? freqs elem)
                          (assoc freqs elem (inc (freqs elem)))
                          (assoc freqs elem 1))]
             (my-frequencies-helper sub-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
      (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
      (let [[amount] (vals a-map)
            [key] (keys a-map)]
           (if (empty? a-map)
             []
             (concat (repeat amount key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
      (if (or (= 0 n) (= 0 (count coll))) '()
        (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
      (cond
        (= n 0) coll
        (= (count coll) 0) '()
        :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
      (let [seq (int (/ (count a-seq) 2))]
           (vector (my-take seq a-seq) (my-drop seq a-seq))))

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

