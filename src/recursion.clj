(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond (empty? a-seq) '()
          (pred? f) (cons f (my-filter pred? r))
          :else (my-filter pred? r))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ['()]
    (map concat
         (rest (reverse (tails a-seq)))
         (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [key (first a-seq)]
    (cond
      (empty? a-seq) freqs
      (contains? freqs key) (my-frequencies-helper (assoc freqs key (inc (get freqs key)))
                                                   (rest a-seq))
      :else (my-frequencies-helper (assoc freqs key 1)
                                   (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elem (first a-map)
        what (first elem)
        how-many (second elem)]
    (if (empty? a-map)
      []
      (concat (repeat how-many what)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    []
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n)
          (empty? coll))
    coll
    (my-drop (dec n)
             (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (vector (my-take h a-seq) (my-drop h a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
            (cons (first a-seq)
                  (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq)
                  (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (let [[a b] (halve a-seq)]
            (seq-merge (merge-sort a)
                       (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

