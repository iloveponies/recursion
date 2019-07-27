(ns recursion)

(defn product [coll]
  (let [looper (fn [acc ns]
                 (if (empty? ns)
                   acc
                   (let [n (first ns)
                         ns' (rest ns)]
                     (recur (* acc n) ns'))))]
    (looper 1 coll)))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (recur (rest coll)))))

(defn max-element [a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (let [x (first xs)
                         xs' (rest xs)]
                     (recur (max acc x) xs'))))]
    (looper (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (cond
     (< len-1 len-2) seq-2
     (< len-2 len-1) seq-1
     :else seq-2)))
;  (first (sort-by count > [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (let [x (first xs)
                         xs' (rest xs)]
                     (recur (seq-max acc x) xs'))))]
    (looper nil a-seq)))
;   (reduce seq-max nil a-seq))

(defn my-filter [pred? a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (let [x (first xs)
                         xs' (rest xs)
                         acc' (if (pred? x) (conj acc x) acc)]
                     (recur acc' xs'))))]
    (looper [] a-seq)))
;   (reduce (fn [acc x] (if (pred? x) (conj acc x) acc)) [] a-seq)

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (let [x (first xs)
                         p (pred? x)
                         xs' (if p (rest xs) [])
                         acc' (if p (conj acc x) acc)]
                     (recur acc' xs'))))]
    (looper [] a-seq)))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (recur pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (if (= (first a-seq) (first b-seq))
           (recur (rest a-seq) (rest b-seq))
           false)))

(defn my-map [f seq-1 seq-2]
  (let [looper (fn [acc xs-1 xs-2]
                 (if (or (empty? xs-1) (empty? xs-2))
                   acc
                   (let [v (f (first xs-1) (first xs-2))]
                     (recur (conj acc v) (rest xs-1) (rest xs-2)))))]
    (looper [] seq-1 seq-2)))

(defn power [n k]
  (let [looper (fn [acc i]
                 (if (zero? i)
                   acc
                   (recur (* acc n) (dec i))))]
    (looper 1 k)))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [looper (fn [acc n]
                 (if (or (zero? n) (neg? n))
                   acc
                   (recur (conj acc what-to-repeat) (dec n))))]
    (looper '() how-many-times)))

(defn my-range [up-to]
  (let [looper (fn [acc n]
                 (if (neg? n)
                   acc
                   (recur (conj acc n) (dec n))))]
    (apply list (looper [] (dec up-to)))))

(defn tails [a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (recur (conj acc xs) (rest xs))))]
    (conj (looper [] (apply list a-seq)) '())))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  ;; (let [looper (fn [acc n xs]
  ;;                (if (zero? n)
  ;;                  acc
  ;;                  (let [xs' (concat (rest xs) (list (first xs)))]
  ;;                    (recur (conj acc xs) (dec n) xs'))))]
  ;;   (if (empty? a-seq)
  ;;     #{'()}
  ;;     (looper #{} (count a-seq) (apply list a-seq)))))
  (let [looper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (conj acc (concat (drop n a-seq)
                                            (take n a-seq)))
                          (dec n))))]
    (if (empty? a-seq)
      #{'()}
      (looper #{} (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          n (get freqs x 0)
          freqs' (assoc freqs x (inc n))]
      (recur freqs' (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [reps (fn [k]
               (let [n (a-map k)]
                 (repeat n k)))]
    (apply concat (map reps (keys a-map)))))

(defn my-take [n coll]
  (let [looper (fn [acc i xs]
                 (if (or (zero? i) (neg? i) (empty? xs))
                   acc
                   (recur (conj acc (first xs))
                          (dec i)
                          (rest xs))))]
    (looper [] n coll)))

(defn my-drop [n coll]
  (cond
   (empty? coll) coll
   (or (zero? n) (neg? n)) coll
   :else (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (count a-seq)
        mid (int (/ n 2))]
    (vector
     (take mid a-seq)
     (drop mid a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [looper (fn [acc xs ys]
                 (cond
                  (empty? xs) (concat acc ys)
                  (empty? ys) (concat acc xs)
                  :else (let [x (first xs)
                              y (first ys)]
                          (if (<= x y)
                            (recur (conj acc x) (rest xs) ys)
                            (recur (conj acc y) xs (rest ys))))))]
    (looper [] a-seq b-seq)))

(defn merge-sort [a-seq]
  (let [n (count a-seq)]
    (if (<= n 1)
      a-seq
      (apply seq-merge (map merge-sort (halve a-seq))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (let [looper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (let [xs-inits (filter (comp not empty?) (inits xs))
                         prefix (last (filter monotonic? xs-inits))
                         n (count prefix)]
                     (recur (conj acc prefix) (drop n xs)))))]
    (looper [] a-seq)))

(defn perm-helper [xs]
  (let [n (count xs)
        vs (vec xs)]
    (cond
     (zero? n) #{}
     (= 1 n) #{vs}
     (= 2 n) #{vs (vec (reverse vs))}
     :else :dunno)))

(defn permutations [a-set]
  []) ; dunno

(defn integer->binary-expansion [i]
  (let [looper (fn [acc z]
                 (if (zero? z)
                   acc
                   (recur (conj acc (odd? (mod z 2)))
                          (int (/ z 2)))))]
    (looper [] i)))

(defn binary-expansion->exponents [bes]
  (let [pairs (map vector bes (range (count bes)))]
    (map (fn [[_ i]] i)
         (filter (fn [[b _]] b)
                 pairs))))

(defn powerset [a-set]
  (let [n (count a-set)
        vs (vec a-set)
        m (apply * (repeat n 2))]
    (map (fn [i]
           (let [exponents (binary-expansion->exponents
                            (integer->binary-expansion i))]
             (map (fn [e]
                    (get vs e))
                  exponents)))
         (range m))))

