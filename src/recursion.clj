(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn singleton-or-empty? [coll]
  (defn singleton-or-empty? [coll]
  (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton-or-empty? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (singleton-or-empty? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (singleton-or-empty? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
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
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

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
  (let [val (dec up-to)]
    (if (<= up-to 0)
      '()
      (cons val (my-range val)))))

(defn tails [a-seq]
  (let [rev (reverse a-seq)
        fun (fn [x]
              (reverse (take x rev)))]
    (map fun (range 0 (inc (count a-seq))))))

(defn inits [a-seq]
  (let [fun (fn [x]
              (take x a-seq))]
    (map fun (range 0 (inc (count a-seq))))))

(defn rotations [a-seq]
  (let [rotate (fn rotate [sequ times]
                 (if (<= times 0)
                   sequ
                   (rotate (cons (last sequ) (butlast sequ)) (dec times))))]
    (if (empty? a-seq)
      '(()) ; To pass tests...
      (map #(rotate a-seq %) (range 1 (inc (count a-seq)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if(empty? a-seq)
     freqs
     (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq) 0))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [head (first a-map)]
      (concat (repeat (second head) (first head)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll)
     coll
   (<= n 0)
     '()
   :else
     (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll)
     coll
   (<= n 0)
     coll
   :else
     (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
    (cond
     (empty? a-seq)
       b-seq
     (empty? b-seq)
       a-seq
     :else
       (if (<= (first a-seq) (first b-seq))
         (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
         (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (singleton-or-empty? a-seq)
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn monotonic? [a-seq]
  (or (empty? a-seq) (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonics (take-while monotonic? (inits a-seq))]
      (cons (last monotonics) (split-into-monotonics (drop (dec (count monotonics)) a-seq))))))

(defn permutations [a-seq]
  (if (singleton-or-empty? a-seq)
    (list a-seq)
    (for [head a-seq
          tail (permutations (disj (set a-seq) head))]
      (do
        (cons head tail)))))

(defn powerset [a-set]
  (let [helper (fn [coll value]
                 (set (concat coll (map #(set (concat % #{value})) coll))))]
    (reduce helper #{#{}} a-set)))

