(ns recursion)

(defn product [coll]
  (if-not (empty? coll)
   (* (first coll) (product (rest coll)))
   1))

;; (product [1 2 3])
;; (* 1 (product [2 3]))
;; (* 1 (* 2 (product [3])))
;; (* 1 (* 2 (* 3 1)))
;; (* 1 (* 2 (* 3 1)))

(defn singleton? [coll]
  (if (= 1 (count coll))
    true
    false))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [f (first a-seq) other (max-element (rest a-seq))]
            (if (> f other) f other))))

(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1))
    seq-2
    seq-1))

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
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
       '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [funk a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq)) '()

   (or (singleton? a-seq) (singleton? b-seq)) (cons (funk (first a-seq) (first b-seq)) '())

   :else (cons (funk (first a-seq) (first b-seq))
               (my-map funk (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (cond
    (= k 0) 1
    (= k 1) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (let [b-seq (rest a-seq)]
    (if (empty? a-seq)
      '(())
      (cons a-seq (tails b-seq)))))

(defn inits [a-seq]
  (let [b-seq (rest (reverse a-seq))]
    (if (empty? a-seq)
      '(())
      (cons a-seq (inits (reverse b-seq))))))

(defn rotations [a-seq]
  (if-not (empty? a-seq)
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))
    '(())))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)
        s (get freqs f)
        v (if-not (number? s) 1 (inc s))]
    (cond
      (empty? a-seq) freqs
      :else (my-frequencies-helper (assoc freqs f v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map #(repeat (second %1) (first %1)) a-map)))

(defn my-take [n coll]
  (if (and (> n 0) (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    '()))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (> n 0) (my-drop (dec n) (rest coll))
    :else (cons (first coll) (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))
        first-half (my-take h a-seq)
        last-half (my-drop h a-seq)]

    (seq [first-half last-half])))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (> a b) (cons b (seq-merge a-seq (rest b-seq)))
      :else (cons a (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) a-seq
    (let [ [h1 h2] (halve a-seq)]
      (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonic? (fn [s] (or (apply <= s) (apply >= s)))
          mono (first (drop-while #(not (monotonic? %)) (inits a-seq)))
          mono-count (count mono)]
        (cons mono (split-into-monotonics (drop mono-count a-seq))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (list a-set)
    (for [ head a-set
           others (permutations (disj (set a-set) head))]
      (cons head others))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{a-set}
    (let [elem (first a-set)
          subset (disj (set a-set) elem)
          power-subset (powerset subset)]
      (clojure.set/union power-subset
                         (map #(conj % elem) power-subset)))))
