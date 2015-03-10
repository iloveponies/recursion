(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(* 1 (* 2 (* 4 1)))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

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
    (let [f (first a-seq)
          filtered-rest (my-filter pred? (rest a-seq))]
      (if (pred? f)
        (cons f filtered-rest)
        filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
   false
   (not (== elem (first a-seq)))
   (sequence-contains? elem (rest a-seq))
   :else
   true))


(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   a-seq
   (pred? (first a-seq))
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   a-seq
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (complement (empty? b-seq))) false
   (and (empty? b-seq) (complement (empty? a-seq))) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (< n 3) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
;=> ()

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (let [up-to-minus-one  (dec up-to)]
      (cons up-to-minus-one (my-range up-to-minus-one)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (set (map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          element-count (inc (if (not (freqs element)) 0 (freqs element)))]
      (my-frequencies-helper (assoc freqs element element-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[x n] (first a-map)]
      (concat (repeat n x) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [first-c (int (/ (count a-seq) 2))]
    [(my-take first-c a-seq) (my-drop first-c a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [first-a (first a-seq)
               first-b (first b-seq)]
           (if (< first-a first-b)
             (cons first-a (seq-merge (rest a-seq) b-seq))
             (cons first-b (seq-merge a-seq (rest b-seq)))
             ))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (= (count a-seq) 1) a-seq
   :else (let [[first-half second-half] (halve a-seq)]
        (seq-merge (merge-sort first-half) (merge-sort second-half))
  )))

(defn monotonic-helper [compare-function coll]
  (if (or (empty? coll) (= (count coll) 1))
    true
    (let [f (first coll)
          s (first (rest coll))]
      (if (compare-function s f)
        (monotonic-helper compare-function (rest coll))
        false))))

(defn monotonic? [coll]
  (or (monotonic-helper < coll) (monotonic-helper > coll)))

(defn split-into-monotonics [a-seq]
  (if (< (count a-seq) 1)
    (seq a-seq)
    (let [mono-head-length (apply max (map count (take-while monotonic? (reverse (inits a-seq)))))]
    (cons (take mono-head-length a-seq) (split-into-monotonics (drop mono-head-length a-seq))))))



(defn permutations [a-set]
  (if (= (count a-set) 2)
    (let [[a b] a-set]
      [[a b] [b a]])
    (map concat (repeat [(first a-set)]) (permutations (rest a-set)))))

(map concat (repeat [5]) [[1,3]])

(rotations #{1 5 3})
(permutations #{1 5 3 4})
(= (permutations #{1 5 3}) '((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1)))


(defn powerset [a-set]
  [:-])

