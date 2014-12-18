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
  (when-not (empty? coll)
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (when-not (empty? a-seq)
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (loop [s1 seq-1 s2 seq-2]
    (cond
     (empty? s1) seq-2
     (empty? s2) seq-1
     :else (recur (rest s1) (rest s2)))))

(defn longest-sequence [a-seq]
  (when-not (empty? a-seq)
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
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq 
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (and (= (first a-seq) (first b-seq))
              (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= n 0) 0
   (= k 0) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
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
    '(())
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [n] (concat (drop n a-seq) (take n a-seq))) (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (merge-with + freqs {(first a-seq) 1}) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[x f] (first a-map)]
      (concat (my-repeat f x) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond 
   (empty? coll) '()
   (<= n 0) (sequence coll)
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [k (int (/ (count a-seq) 2))]
    (vector (take k a-seq) (drop k a-seq))))

(defn seq-merge [a-seq b-seq]
  (loop [s1 a-seq s2 b-seq res '()]
    (cond
     (empty? s1) (concat (reverse res) s2)
     (empty? s2) (concat (reverse res) s1)
     :else (let [a (first s1)
                 b (first s2)]
             (if (< a b)
               (recur (rest s1) s2 (cons a res))
               (recur s1 (rest s2) (cons b res)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (sequence a-seq)
    (let [[as bs] (halve a-seq)]
      (seq-merge (merge-sort as) (merge-sort bs)))))

(defn split-into-monotonics [a-seq]
  (letfn [(monotonic? [xs]
            (or (empty? xs)
                (apply <= xs)
                (apply >= xs)))]
    (if (empty? a-seq)
      '()
      (let [as (last (my-take-while monotonic? (inits a-seq)))
            bs (my-drop (count as) a-seq)]
        (cons as (split-into-monotonics bs))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat (fn [x] (map #(cons x %) (permutations (disj (set a-set) x)))) a-set)))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [x (first a-set)
          xs (rest a-set)
          ss (powerset xs)]
      (clojure.set/union ss (into #{} (map #(conj % x) ss))))))
