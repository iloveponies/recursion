(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product(rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (next coll))))

(defn my-last [coll]
  (if (empty? (next coll))
    (first coll)
    (recur (next coll))))

(defn max-element [a-seq]
  (loop [coll (next a-seq), acc (first a-seq)]
    (if (nil? (first coll))
      acc
      (recur (next coll) (max acc (first coll))))))

(defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2))
         seq-1
         seq-2))

(defn seq-max-r [s1 s2]
  (loop [r1 s1, r2 s2]
    (cond
     (empty? r2) s1
     (empty? r1) s2
     :else (recur (next r1) (next r2)))))

(defn longest-sequence [a-seq]
  (loop [acc-seq nil, colls a-seq]
    (if (empty? colls)
      acc-seq
      (recur (seq-max acc-seq (first colls)) (next colls)))))

(defn my-filter [pred? coll]
   (cond (empty? coll)
         '()
         (pred? (first coll))
         (cons (first coll) (lazy-seq (my-filter pred? (next coll))))
         :else (lazy-seq (my-filter pred? (next coll)))))

(defn sequence-contains? [elem a-seq]
  (loop [coll a-seq]
    (cond (empty? coll) false
          (= elem (first coll)) true
          :else (recur (next coll)))))

(defn my-take-while [pred? coll]
  (cond (empty? coll) '()
        (not (pred? (first coll))) '()
        :else (cons (first coll) (lazy-seq (my-take-while pred? (next coll))))))

(defn my-drop-while [pred? coll]
  (cond (empty? coll) '()
        (not (pred? (first coll))) coll
        :else (recur pred? (next coll))))

(defn seq= [seq-a seq-b]
  (let [empty-a? (empty? seq-a)
        empty-b? (empty? seq-b)
        ]
    (cond
        (and empty-a? empty-b?) true
        (or empty-a? empty-b?)  false
        (not (= (first seq-a) (first seq-b))) false
        :else (recur (next seq-a) (next seq-b))
        )))

(defn my-map  [f seq-1 seq-2]
  (let [f1 (first seq-1)
        f2 (first seq-2)]
    (if
      (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f f1 f2) (lazy-seq (my-map f (next seq-1) (next seq-2)))))))


(defn power  "calculate power of n to k"(
  [n k]
  (power n k 1))
  ([n k acc]
   (if (< k 1)
     acc
     (recur n (dec k) (* acc n)))))

(defn fib " calculate fibonnaci number n"
  ([n] (fib n 0 1))
  ([n f-nth a]
   (if (< n 1)
     f-nth
     (recur (dec n) (+ f-nth a) f-nth))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (lazy-seq (my-repeat (dec how-many-times) what-to-repeat)))))

(defn my-range [up-to]
  (let [val (dec up-to)]
    (if (< up-to 1)
      '()
      (cons val (lazy-seq (my-range val))))))

(defn tails [coll] "create sequence of all subsets from tail back"
  (if (empty? coll)
    '(())
    (cons (seq coll) (tails (next coll)))))

(defn inits
  "create sequence of all subsets from start forward"
  ([coll]
   (map #(take % coll) (range (inc (count coll))))))

(defn rotations
  ([x]
   (if (empty? x)
     '(())
     (rotations x (count x))))
  ([x n]
   (if (< n 1)
     '()
     (cons x (rotations (concat (next x) (list (first x))) (dec n))))))

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

