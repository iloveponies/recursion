(ns recursion)

(defn product
  [coll]
  (reduce * coll))

(defn singleton?
  [coll]
  (if (or (empty? coll) (seq (rest coll)))
    false
    true))

(defn my-last
  [coll]
  (when (seq coll)
    (reduce (fn [_ x]
              x)
            coll)))

(comment
  (defn max-element
    [a-seq]
    (when (seq a-seq)
      (apply max a-seq)))
  )

(defn max-element
  [a-seq]
  (letfn [(max-element* [acc coll]
            (if-let [s (seq coll)]
              (let [[x & xs] s]
                (recur (max acc x) xs))
              acc))]
    (max-element* (first a-seq) (rest a-seq))))

(defn seq-max
  [seq-1 seq-2]
  (let [n1 (count seq-1)
        n2 (count seq-2)]
    (if (> n1 n2)
      seq-1
      seq-2)))

(defn longest-sequence
  [a-seq]
  (when (seq a-seq)
    (reduce (fn [acc x]
              (if (< (count acc) (count x))
                x
                acc))
            a-seq)))

(defn my-filter
  [pred? a-seq]
  (when-let [s (seq a-seq)]
    (let [[x & xs] s]
      (if (pred? x)
        (cons x (lazy-seq (my-filter pred? xs)))
        (lazy-seq (my-filter pred? xs))))))

(defn sequence-contains?
  [elem a-seq]
  (not (nil?
         (loop [coll a-seq]
           (when-let [s (seq coll)]
             (if (= (first s) elem)
               elem
               (recur (rest s))))))))

(defn my-take-while
  [pred? a-seq]
  (if-let [s (seq a-seq)]
    (let [[x & xs] s]
      (if (pred? x)
        (cons x (my-take-while pred? xs))
        ()))
    ()))

(defn my-drop-while
  [pred? a-seq]
  (if-let [s (seq a-seq)]
    (let [[x & xs] s]
      (if (pred? x)
        (my-drop-while pred? xs)
        s))
    ()))

(defn seq=
  [a-seq b-seq]
  (= a-seq b-seq))

(defn my-map
  [f seq-1 seq-2]
  (let [s1 (seq seq-1)
        s2 (seq seq-2)]
    (if (and s1 s2)
      (cons (f (first s1) (first s2))
            (my-map f (rest s1) (rest s2)))
      ())))

(defn power
  [n k]
  (product (repeat k n)))

(defn fib
  [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat
  [how-many-times what-to-repeat]
  (map (constantly what-to-repeat) (range how-many-times)))

(defn my-range
  [up-to]
  (take-while (complement neg?) (iterate dec (dec up-to))))

(defn tails
  [a-seq]
  (concat (take-while seq (iterate rest a-seq)) '(())))

(defn inits
  [a-seq]
  (cons () (reverse (take-while seq (iterate butlast a-seq)))))

(defn rotations
  [a-seq]
  (if (seq a-seq)
    (let [n (count a-seq)]
      (take n (partition n 1 (cycle a-seq))))
    '(())))

; XXX: Why would I need this?
(defn my-frequencies-helper
  [freqs a-seq]
  [:-])

(defn my-frequencies
  [a-seq]
  (reduce (fn [acc x]
            (assoc acc x (inc (get acc x 0))))
          {}
          a-seq))

(defn un-frequencies
  [a-map]
  (mapcat (fn [[k v]]
            (repeat v k))
          a-map))

(defn my-take
  [n coll]
  (loop [i 0 acc [] coll coll]
    (if (or (<= n i) (empty? coll))
      acc
      (recur (inc i) (conj acc (first coll)) (rest coll)))))
  
(defn my-drop 
  [n coll]
  (loop [i 0 coll coll]
    (cond (empty? coll) ()
          (<= n i) coll
          :else (recur (inc i) (rest coll)))))

(defn halve
  [a-seq]
  (split-at (int (/ (count a-seq) 2)) a-seq))

(defn seq-merge
  [a-seq b-seq]
  (loop [acc [] c1 a-seq c2 b-seq]
    (let [[x & xs] c1
          [y & ys] c2]
      (cond (and x y) (if (< x y)
                        (recur (conj acc x) xs c2)
                        (recur (conj acc y) c1 ys))
            x (concat acc c1)
            y (concat acc c2)
            :else []))))

(defn merge-sort
  [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[xs ys] (halve a-seq)]
      (seq-merge (merge-sort xs) (merge-sort ys)))))

(defn monotonic?
  [coll]
  (or (apply <= coll)
      (apply >= coll)))

(defn split-into-monotonics
  [a-seq]
  (loop [mono []
         acc []
         coll a-seq]
    (cond (empty? coll) (conj acc mono)
          (monotonic? (conj mono (first coll))) (recur (conj mono (first coll))
                                                       acc
                                                       (rest coll))
          :else (recur [] (conj acc mono) coll))))
  
(defn permutations
  [a-set]
  (if (empty? a-set)
    [[]]
    (for [x a-set
          ys (permutations (disj (set a-set) x))]
      (conj ys x))))

(defn powerset
  [a-set]
  (set (reduce (fn [acc x]
                 (concat acc (map #(conj % x) acc)))
               #{#{}}
               a-set)))

