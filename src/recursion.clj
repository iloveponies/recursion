(ns recursion)

(defn product [coll]
  (cond (empty? coll) 1
        (empty? (rest coll)) (first coll)
        :else (recur (cons (* (first coll) (second coll))
                           (rest (rest coll))))))

(defn singleton? [coll]
  (boolean (and (seq coll)
                (empty? (rest coll)))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (recur (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (recur (cons (max (first a-seq) (second a-seq))
                           (rest (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (max-key count seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (recur (cons (seq-max (first a-seq) (second a-seq))
                           (rest (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (recur pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (recur elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    []
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (recur pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (if (and (seq a-seq)
           (seq b-seq))
    (if (not= (first a-seq)
              (first b-seq))
      false
      (recur (rest a-seq) (rest b-seq)))
    (and (empty? a-seq) (empty? b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    []))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [shrinking-tails (tails a-seq)
          growing-inits (inits a-seq)]
      (rest (map concat shrinking-tails growing-inits)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [curr-elem (first a-seq)]
      (recur (assoc freqs curr-elem
                    (inc (get freqs curr-elem 0)))
             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [a-key (first (keys a-map))]
      (concat (my-repeat (a-map a-key) a-key)
              (un-frequencies (dissoc a-map a-key))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    []
    (if (<= n 0)
      coll
      (recur (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [a-seq-len (count a-seq)
        first-size (int (/ a-seq-len 2))]
    [(my-take first-size a-seq) (my-drop first-size a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (if (<= (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[first-part second-part] (halve a-seq)]
      (seq-merge (merge-sort first-part) (merge-sort second-part)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [ascending (my-take-while #(apply <= %) (remove empty? (inits a-seq)))
          descending (my-take-while #(apply >= %) (remove empty? (inits a-seq)))
          longer-monotonic (last (seq-max ascending descending))]
      (cons longer-monotonic (split-into-monotonics
                              (drop (count longer-monotonic) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (for [elem a-set
          permutation-without-elem (permutations (remove #{elem} a-set))]
      (cons elem permutation-without-elem))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [elem (first a-set)
          powerset-without-elem (powerset (remove #{elem} a-set))]
      (concat
       powerset-without-elem
       (map #(conj % elem) powerset-without-elem)))))
