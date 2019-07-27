(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   ((complement pred?) (first a-seq)) '()
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   ((complement pred?) (first a-seq)) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (let [empty-1 (empty? seq-1)
        empty-2 (empty? seq-2)]
    (if (or empty-1 empty-2)
     '()
     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [n how-many-times
        x what-to-repeat]
    (if (<= n 0)
      '()
      (cons x (my-repeat (dec n) x)))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (map reverse (cons (reverse a-seq)
                       (tails (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          new-freqs (if (contains? freqs item)
                      (assoc freqs item (inc (freqs item)))
                      (assoc freqs item 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [v (second (first a-map))
          k (first (first a-map))]
      (concat (repeat v k)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [cnt (count a-seq)
        half (int (/ cnt 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [f-a (first a-seq)
        f-b (first b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= f-a f-b) (cons f-a (seq-merge (rest a-seq) b-seq))
     :else (cons f-b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
   (seq-merge (merge-sort (first (halve a-seq)))
              (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics-helper [a-vec a-seq]
  (let [monotonic? (fn [a-seq]
                     (or (apply <= a-seq)
                         (apply >= a-seq)))]
    (if (empty? a-seq)
      a-vec
      (let [next-a-vec (last (take-while monotonic? (drop 1 (reverse (inits a-seq)))))
            next-a-seq (drop (count next-a-vec) a-seq)]
        (split-into-monotonics-helper (conj a-vec next-a-vec) next-a-seq)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper [] a-seq))

(defn permute [so-far remainder]
  (if (empty? remainder)
    [so-far]
    (apply concat (map (fn [n] (permute (cons n so-far) (remove #(= % n) remainder)))
                       remainder))))

(defn permutations [a-set]
    (permute [] a-set))

(defn powerset-helper [accum superset]
  (if (empty? superset)
    accum
    (reduce powerset-helper (conj accum superset) (map
                                                    (partial disj superset)
                                                    superset))))

(defn powerset [a-set]
  (powerset-helper #{#{}} (set a-set)))

