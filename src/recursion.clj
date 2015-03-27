(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
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
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons (+ (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (assoc freqs
                           (first a-seq)
                           (inc (get freqs (first a-seq) 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [elem (first a-map)]
      (concat (repeat (second elem) (first elem)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonic (last (take-while (fn [x]
                                        (or (empty? x) (apply <= x) (apply >= x)))
                                      (inits a-seq)))
          remaining (drop (count monotonic) a-seq)]
      (cons monotonic (split-into-monotonics remaining)))))

(defn permutations [a-set]
  [])

(defn powerset [a-set]
  [])

;; Tidy up
(defn permutations [a-set]
  (let [permutate-rest (fn [x] (let [prefix (first x)
                                     suffixes (permutations (rest x))]
                                 (map (fn [suffix] (cons prefix suffix)) suffixes)))]
    (if (empty? a-set)
      [a-set]
      (apply concat (map permutate-rest (rotations a-set))))))

;; Tidy up
(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [next-elem (first a-set)
          powerset-rest (powerset (rest a-set))]
      (clojure.set/union powerset-rest
                         (map (fn [ps] (conj ps next-elem)) powerset-rest)))))

