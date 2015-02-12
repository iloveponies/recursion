(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not (empty? coll)))
    true
    false))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
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
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
        (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
        (my-take-while pred? (rest a-seq)))
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
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (== 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper 
      (update-in freqs [(first a-seq)] (fnil inc 0)) 
      (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [kv (first a-map)]
      (concat (repeat (val kv) (key kv)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== 0 n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (nthrest a-seq n)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (or (== (count a-seq) 0) (== (count a-seq) 1))
    a-seq
    (let [halved (halve a-seq)]
      (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [x] (or (apply <= x) (apply >= x)))
    monotonic-piece (last (take-while monotonic? (rest (inits a-seq))))]
    (if (or (== (count a-seq) 0) (== (count a-seq) 1))
    a-seq
    (cons monotonic-piece (split-into-monotonics (drop (count monotonic-piece) a-seq))))))

(defn permutations [a-set]
  (if (or (== (count a-set) 0) (== (count a-set) 1))
    (list a-set)
    (apply concat 
      (map (fn [x] (map (fn [perm] (cons (first x) perm)) (permutations (rest x)))) (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [p-set (powerset (rest a-set))]
      (clojure.set/union 
        p-set
        (map (fn [x] (clojure.set/union x #{(first a-set)})) p-set)))))

