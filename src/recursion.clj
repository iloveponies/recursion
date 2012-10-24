(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (== (count coll) 1))

(defn my-last [coll]
  (cond
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else
      (let [x1 (first a-seq)
            x2 (first (rest a-seq))
            xs (rest (rest a-seq))]
        (longest-sequence (cons (seq-max x1 x2) xs)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        ()
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= (first a-seq) elem) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
    :else                               false))

(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    ()
    (cons (f (first a-seq) (first b-seq)) (my-map f (rest a-seq) (rest b-seq)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (neg? n)  nil
    (zero? n) 0
    (= n 1)   1
    :else     (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (not (pos? how-many-times))
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (not (pos? up-to))
    ()
    (let [n (- up-to 1)]
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [len (count a-seq)
        rotate (fn [n] (concat (take-last (- len n) a-seq) (take n a-seq)))]
    (if (zero? len)
      (cons () ())
      (map rotate (range len)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (if (contains? freqs k)
              (get freqs k)
              0)]
      (my-frequencies-helper (assoc freqs k (+ v 1)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map #(apply repeat (reverse %)) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (zero? n)     coll
    :else         (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (let [a (first a-seq)
            m (min a (first b-seq))]
        (if (= m a)
          (cons m (seq-merge (rest a-seq) b-seq))
          (cons m (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic? (fn [s] (or (seq= s (sort < s)) (seq= s (sort > s))))
          x (last (take-while monotonic? (inits a-seq)))
          xs (drop (count x) a-seq)]
      (cons x (split-into-monotonics xs)))))

(defn permutations [a-set]
  :-)


(defn powerset [a-set]
  [:-])

