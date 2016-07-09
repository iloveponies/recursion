(ns recursion)

(defn product-r [acc coll]
  (if (empty? coll)
    acc
    (recur (* (first coll) acc) (rest coll))))

(defn product [coll]
  (product-r 1 coll))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (empty? (rest coll)) (first coll)
    :else (recur (rest coll))))

(defn max-element-r [prev-max a-seq-rest]
  (if (empty? a-seq-rest)
    prev-max
    (recur (max prev-max (first a-seq-rest)) (rest a-seq-rest))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (max-element-r (first a-seq) (rest a-seq))))

(defn seq-max-r [seq-1-rest seq-2-rest seq-1 seq-2]
  (cond
    (empty? seq-1-rest) seq-2
    (empty? seq-2-rest) seq-1
    :else (recur (rest seq-1-rest) (rest seq-2-rest) seq-1 seq-2)))

(defn seq-max [seq-1 seq-2]
  (seq-max-r seq-1 seq-2 seq-1 seq-2))

(defn longest-seq-r [prev-max a-seq-rest]
  (if (empty? a-seq-rest) prev-max
    (recur (seq-max prev-max (first a-seq-rest)) (rest a-seq-rest))))

(defn longest-sequence [a-seq]
  (longest-seq-r nil a-seq))

(defn seq-or-empty [a-seq] (or (seq a-seq) `()))

(defn my-filter-r [seq-f pred? a-seq-rest]
  (let [a-first (first a-seq-rest)]
    (cond
      (empty? a-seq-rest) (seq-or-empty seq-f)
      (pred? a-first) (recur (conj (vec seq-f) a-first) pred? (rest a-seq-rest))
      :else (recur seq-f pred? (rest a-seq-rest)))))

(defn my-filter [pred? a-seq]
  (my-filter-r [] pred? a-seq))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (recur elem (rest a-seq))))

(defn my-take-while-r [seq-t pred? a-seq-rest]
  (let [a-first (first a-seq-rest)]
    (cond
      (empty? a-seq-rest) (seq-or-empty seq-t)
      (pred? a-first) (recur (conj (vec seq-t) a-first) pred? (rest a-seq-rest))
      :else (seq-or-empty seq-t))))

(defn my-take-while [pred? a-seq]
  (my-take-while-r [] pred? a-seq))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) `()
    (pred? (first a-seq)) (recur pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
    :else false))

(defn my-map-r [seq-r f seq-1-rest seq-2-rest]
  (if (or (empty? seq-1-rest) (empty? seq-2-rest))
    (seq-or-empty seq-r)
    (recur (conj
             seq-r
             (f (first seq-1-rest) (first seq-2-rest)))
           f
           (rest seq-1-rest)
           (rest seq-2-rest))))

(defn my-map [f seq-1 seq-2]
  (my-map-r [] f seq-1 seq-2))

(defn power-r [res n k-prev]
  (if (zero? k-prev)
    res
    (recur (* res n) n (dec k-prev))))

(defn power [n k]
  (power-r 1 n k))

(defn fib-r [fib-2 fib-1 count-down]
  (let [fib-n (+ fib-1 fib-2)]
    (if (zero? count-down)
      fib-n
      (recur fib-1 fib-n (dec count-down)))))

(defn fib [n]
  (cond
    (<= n 0) 0
    (<= n 2) 1
    :else (fib-r 1 1 (- n 3))))

(defn my-repeat-r [seq-r count-down what]
  (if (<= count-down 0)
    seq-r
    (recur (cons what seq-r) (dec count-down) what)))

(defn my-repeat [how-many-times what-to-repeat]
    (my-repeat-r `() how-many-times what-to-repeat))

(defn my-range-r [seq-r curr up-to]
  (let [next-curr (inc curr)]
    (if (>= curr up-to)
      seq-r
      (recur (cons curr seq-r) next-curr up-to))))

(defn my-range [up-to]
  (if (< up-to 0)
    `()
    (my-range-r `() 0 up-to)))

(defn tails-r [seq-r a-seq-rest]
  (if (empty? a-seq-rest)
    (cons a-seq-rest seq-r)
    (recur (cons a-seq-rest seq-r) (rest a-seq-rest))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (tails-r `() a-seq)))

(defn inits-r [seq-r seq-last a-seq-rest]
  (if (empty? a-seq-rest)
    seq-r
    (let [seq-next (conj seq-last (first a-seq-rest))]
      (recur (conj seq-r seq-next) seq-next (rest a-seq-rest)))))

(defn inits [a-seq]
  (inits-r [[]] [] a-seq))

(defn rotations-r [seq-r seq-last seq-rest]
  (if (empty? seq-rest)
    seq-r
    (let [seq-next (conj (subvec seq-last 1) (first seq-last))]
      (recur (cons seq-next seq-r) seq-next (rest seq-rest)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-r [] (vec a-seq) a-seq)))

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

