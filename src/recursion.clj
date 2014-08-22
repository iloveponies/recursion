(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq)     nil
        (singleton? a-seq) (first a-seq)
        :else              (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq)     nil
        (singleton? a-seq) (first a-seq)
        :else              (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? head)  (cons head
                         (my-filter pred? tail))
     :else         (my-filter pred? tail))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)         false
   (= elem (first a-seq)) true
   :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? head)   (cons head
                          (my-take-while pred? tail))
     :else          '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)        a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq))  false
   (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
   :else                               false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1)
       (empty? seq-2)) '()
   :else               (cons (f (first seq-1) (first seq-2))
                             (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (let [n (dec up-to)]
      (cons n
            (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (concat (inits (drop-last a-seq))
            (list (seq a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map-indexed (fn [i _] (concat (drop i a-seq) (take i a-seq)))
                 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          freq (get freqs elem 0)]
      (my-frequencies-helper (assoc freqs elem (inc freq))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[elem n] (first a-map)]
      (concat (repeat n elem)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq)                      b-seq
   (empty? b-seq)                      a-seq
   :else                               (let [a      (first a-seq)
                                             b      (first b-seq)
                                             lower  (min a b)
                                             a-rest (if (< a b) (rest a-seq) a-seq)
                                             b-rest (if (< a b) b-seq (rest b-seq))]
                                         (cons lower (seq-merge a-rest b-rest)))))

(defn merge-sort [a-seq]
  (if (<= 0 (count a-seq) 1)
    a-seq
    (let [in-half (halve a-seq)]
      (seq-merge (merge-sort (first in-half))
                 (merge-sort (last in-half))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [diff      (apply - (take 2 a-seq))
          f         (if (pos? diff) > <)
          monotonic (last (take-while (partial apply f)
                                      (rest (inits a-seq))))]
      (cons monotonic
            (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat (fn [x]
              (map (fn [y] (cons x y))
                   (permutations (disj (set a-set) x))))
            a-set)))

(defn powerset [a-set]
  [:-])

