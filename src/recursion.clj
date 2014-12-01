(ns recursion)

(defn product [coll]
  (apply * coll))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (let [my-last' (fn [xs x]
                   (if (empty? xs)
                     x
                     (recur (rest xs) (first xs))))]
    (my-last' coll nil)))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? fst)
        (cons fst (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (let [fst (first a-seq)]
    (cond
     (empty? a-seq) false
     (= fst elem) true
     :else (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? fst) (cons fst (my-take-while pred? (rest a-seq)))
     :else nil)))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))   true
   (not (= (count a-seq) (count b-seq))) false
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (let [power' (fn [n k res]
                 (if (= 0 k)
                   res
                   (recur n (dec k) (* n res))))]
    (power' n k 1)))

(defn fib [n]
  (let [fib' (fn [m res1 res2]
               (if (= 0 m)
                 res1
                 (recur (dec m) res2 (+ res1 res2))))]
    (fib' n 0 1)))

(defn my-repeat [n x]
  (let [my-repeat' (fn [m y res]
                     (if (>= 0 m)
                       res
                       (recur (dec m) y (cons y res))))]
    (my-repeat' n x '())))

(defn my-range [up-to]
  (let [my-range' (fn [lim res]
                    (if (<= lim 0)
                      res
                      (recur (dec lim) (cons (dec lim) res))))]
    (my-range' up-to '())))

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

