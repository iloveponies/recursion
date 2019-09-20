(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll)
          (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [count1 (count seq-1)
        count2 (count seq-2)]
    (if (> count1 count2)
      seq-1
      seq-2)))

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
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not (= (or (first a-seq) 0) (or (first b-seq)))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) '()
   (cons
    (f (first seq-1) (first seq-2))
    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if
    (empty? a-seq) '(())
    (cons (concat a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [my-butlast
    (fn [x] (reverse (rest (reverse x))))]
  (if
    (empty? a-seq) '(())
    (cons (concat a-seq) (inits (my-butlast a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-count (or (get freqs (first a-seq)) 0)
          new-freqs (assoc freqs (first a-seq) (inc current-count))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    {}
    (let [v (first (first a-map))
          c (second (first a-map))]
      (concat (repeat c v)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (<= n 0)
      (cons (first coll) (my-drop n (rest coll)))
      (my-drop (dec n)(rest coll)))))

(defn halve [a-seq]
  (let [halfcount (int (/ (count a-seq) 2))]
  (vector (my-take halfcount a-seq) (my-drop halfcount a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= a b) (cons a (seq-merge (rest a-seq) b-seq))
     :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [len (count a-seq)
        halves (halve a-seq)
        first-half (first halves)
        second-half (second halves)]
    (if (or (= 1 len) (zero? len))
      a-seq
      (seq-merge (merge-sort first-half)
               (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

