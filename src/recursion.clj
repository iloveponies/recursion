(ns recursion)

(defn product [coll]
  ; (reduce * coll))
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn- recursion-fn [f g coll]
  (when-not (empty? coll)
    (if (singleton? coll)
      (first coll)
      (f (g coll)))))

(defn my-last [coll]
  (recursion-fn my-last rest coll))

(defn max-element [a-seq]
  ; (apply max a-seq)
  (let [max-fn (fn [[x1 x2 & xs]] (cons (max x1 x2) xs))]
    (recursion-fn max-element max-fn a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [max-fn (fn [[x1 x2 & xs]] (cons (seq-max x1 x2) xs))]
    (recursion-fn longest-sequence max-fn a-seq)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
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
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (condp = n
    0 0
    1 1
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons (vec a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons (vec a-seq) (inits (butlast a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (get freqs k 0)]
      (my-frequencies-helper (assoc freqs k (inc v)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (>= n (count coll))
    coll
    (if (= n 0)
      '()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size (count a-seq)
        first-half-size (int (/ size 2))]
    [(my-take first-half-size a-seq) (my-drop first-half-size a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (>= (count a-seq) 2)
    (apply seq-merge (map merge-sort (halve a-seq)))
    a-seq))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [s]
                     (or (apply <= s)
                         (apply >= s)))
        last-monotonic (last (take-while monotonic? (filter #(> (count %) 1) (reverse (inits a-seq)))))]
    (if last-monotonic
      (cons last-monotonic (split-into-monotonics (drop (count last-monotonic) a-seq)))
      '())))

(defn perm-helper [set-1 set-2]
  (if (empty? set-2)
    set-1
    (mapcat (fn [k] (perm-helper (conj set-1 k) (disj set-2 k))) set-2)))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (partition (count a-set) (perm-helper '() (set a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [true-set (set a-set)]
      (set (cons true-set (mapcat #(powerset (disj true-set %)) true-set))))))
