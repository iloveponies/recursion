(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

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
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0
   (zero? k) 1
   :else (* n (power n (dec k)))))

; exponential definition
(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [rev-seq (reverse a-seq)]
    (if (empty? a-seq)
      '([])
      (reverse (cons a-seq (inits (reverse (rest rev-seq))))))))

(defn rotations [a-seq]
  (let [go (defn go [n b-seq]
             (if (<= n 0)
               '()
               (cons b-seq (go (dec n) (concat (rest b-seq) [(first b-seq)])))))]
    (if (empty? a-seq)
      '(())
      (go (count a-seq) a-seq))))

(defn update [m k v f]
  (if (contains? m k)
    (assoc m k (f (m k)))
    (assoc m k v)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          tail (rest a-seq)]
      (my-frequencies-helper (update freqs head 1 inc) tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[key times] (first a-map)
          tail        (rest  a-map)]
      (concat (repeat times key) (un-frequencies tail)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-head (first a-seq)
        b-head (first b-seq)
        a-tail (rest a-seq)
        b-tail (rest b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (> a-head b-head) (cons b-head (seq-merge a-seq b-tail))
     :else (cons a-head (seq-merge a-tail b-seq)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn inits2 [coll]
  (let [n (count coll)
        go (fn [m] (take m coll))]
    (map go (range (inc n)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [first-monos (take-while monotonic? (inits2 a-seq))
          len-first-monos (count first-monos)
          longest-mono (first (drop (dec len-first-monos) first-monos))]
      (cons longest-mono (split-into-monotonics (drop (dec len-first-monos) a-seq))))))

(defn permutations [a-set]
  (cond
   (empty? a-set) [[]]
   (singleton? a-set) [a-set]
   (singleton? (rest a-set)) (let [[a b] a-set]
                               [[a b] [b a]])
   :else (let [add-to-front (fn [elem list-of-lists]
                              (map #(cons elem %) list-of-lists))
               go (fn [[head tail]]
                    (add-to-front head (permutations tail)))
               rots (rotations a-set)
               headtail (fn [coll] [(first coll) (rest coll)])]
           (mapcat go (map headtail rots)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [head (first a-set)
          tail (rest a-set)
          tail-powerset (powerset tail)
          tail-powerset-with-head (map #(conj % head) tail-powerset)]
      (concat tail-powerset tail-powerset-with-head))))

