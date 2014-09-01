(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
         (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else
       '()
   ))

 (defn my-drop-while [pred? a-seq]
  (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (my-drop-while pred? (rest a-seq))
     :else
       a-seq))

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq)
          (empty? b-seq))
       true
     (and (seq a-seq)
          (seq b-seq)
          (= (first a-seq)
             (first b-seq)))
       (seq= (rest a-seq)
             (rest b-seq))
     :else
       false
    ))

(defn my-map [f a-seq b-seq]
   (if (or (empty? a-seq)
            (empty? b-seq))
       '()
     (cons (f (first a-seq)
              (first b-seq))
       (my-map f (rest a-seq)
                 (rest b-seq)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (> n 1)
    (+ (fib (dec n))
       (fib (dec (dec n))))
    n))

(defn my-repeat [n what-to-repeat]
  (if (< n 1)
    '()
    (cons what-to-repeat (my-repeat (dec n) what-to-repeat))
    ))

(defn my-range [n]
  (if (zero? n)
    '()
    (cons (dec n) (my-range (dec n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '[[]]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (loop [i []
         acc [[]]
         the-seq a-seq]
    (if (empty? the-seq)
      acc
      (let [newi (conj i (first the-seq))]
        (recur
           newi
           (conj acc newi)
           (rest the-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (loop [a []
           b a-seq
           acc []]
      (if (empty? b)
        acc
        (let [f (first b)
              r (rest b)]
          (recur (conj a f)
                 r
                 (conj acc (concat b a))))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          sofar (get freqs el 0)
          new-freqs (assoc freqs el (inc sofar))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat '() (map (fn [[k v]] (repeat v k)) a-map)))

(defn my-take [n coll]
  (cond
     (empty? coll)
       '()
     (> n 0)
       (cons (first coll) (my-take (dec n) (rest coll)))
     :else
       '()
   ))

(defn my-drop [n a-seq]
  (cond
     (empty? a-seq)
       '()
     (> n 0)
       (my-drop (dec n) (rest a-seq))
     :else
       a-seq))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq)
     (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   :else
     (let [fa (first a-seq)
           fb (first b-seq)]
       (if (< fa fb)
         (cons fa (seq-merge (rest a-seq) b-seq))
         (cons fb (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (nil? (first a-seq))
          (nil? (next  a-seq)))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (loop [acc []
           mon []
           l (first a-seq)
           the-seq a-seq]
      (if (empty? the-seq)
        (conj acc mon)
        (let [f (first the-seq)]
          (if (<= l f)
            (recur acc (conj mon f) f (rest the-seq))
            (recur (conj acc mon) [f] f (rest the-seq))))))))

(defn ins [v a]
  (let [n (count v)]
    (for [i (range (inc n))]
      (concat (conj (vec (take i v)) a) (drop i v)))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (let [v (vec a-set)]
      (loop [acc [[]]
             a-seq v]
        (if (empty? a-seq)
          acc
          (recur (reduce concat (map (fn [x] (ins x (first a-seq))) acc))
                 (rest a-seq)))))))

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn powerset [a-set]
  (let [n (Math/pow 2 (count a-set))
        lst (indexed a-set)]
    (map (fn [i] (set (map second (filter (fn [[idx val]]
                                            (bit-test i idx))
                                          lst))))
         (range 0 n))))
