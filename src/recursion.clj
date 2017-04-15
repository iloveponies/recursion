(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(product (cons 1 (cons 2 (cons 4 '()))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
   (and (empty? seq-1) (empty? seq-2)) nil
   (empty? seq-1) seq-2
   (empty? seq-2) seq-1
   :else (if (> (count seq-1) (count seq-2)) seq-1 seq-2)))


(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   :else (if (pred? (first a-seq))
     (cons (first a-seq) (my-filter pred? (rest a-seq)))
     (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   (sequence-contains? elem (rest a-seq)) true
   :else false))

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
   (= a-seq b-seq))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) '()
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0
   (zero? k) 1
   :else (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
    (if (empty? a-seq)
      (conj () ())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (tails a-seq)
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq)
                             (if (contains? freqs (first a-seq))
                               (inc (get freqs (first a-seq)))
                               1))
                             (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [jj (int (/ (count a-seq) 2))]
  (conj []
  (my-take jj a-seq)
  (my-drop jj a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [x] (if (empty? x) false
                             (or (apply <= x)
                                 (apply >= x))))
        longest-m (last (filter monotonic? (inits a-seq)))]
    (if (empty? a-seq)
      []
      (cons longest-m (split-into-monotonics (drop (count longest-m) a-seq))))))

(defn rotate-a-rotation [a-seq]
  (if (> 3 (count a-seq))
    (rotations a-seq)
  (let [rota1 (first (rotations a-seq))
        half1 (first (halve rota1))
        half2 (first (rotations (last (halve rota1))))]
    (conj '()
          rota1
          (concat half1 half2)))))

(defn permutations-for-set-size-under-4-huoh [a-set]
  (if (> 3 (count a-set))
    (rotations a-set)
    (apply concat (map rotate-a-rotation (rotations a-set)))))

(defn permutations [a-set]
(if (< (count a-set) 3)
  (rotations a-set)
  (apply concat (map rotations (map (fn [x] (cons (first a-set) x)) (permutations (rest a-set)))))))

(defn powerset [a-set]
  :-)

