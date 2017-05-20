(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq) )))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times) []
    (= 1 how-many-times) (vector what-to-repeat)
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) []
    (= 1 up-to) (vector 0)
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [a-seq]
  (concat (rest a-seq) (vector (first a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (for [i (range (count a-seq))]
      (concat (drop i a-seq) (take i a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (loop [freqs freqs
         a-seq a-seq]
    (let [element (first a-seq)
          freq (or (get freqs element) 0)]
    (if (seq a-seq)
      (recur (assoc freqs element (inc freq)) (rest a-seq))
      freqs))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map repeat (vals a-map) (keys a-map))))

(defn my-take [n coll]
  (cond
    (or (<= n 0) (not (seq coll))) []
    (= 1 n) (vector (first coll))
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (let [n-minus-count (- (count coll) n)
        reversed-coll (reverse coll)]
    (reverse (my-take n-minus-count reversed-coll))))

(defn halve [a-seq]
  (let [midway-count (int (/ (count a-seq) 2))]
    (vector (my-take midway-count a-seq) (my-drop midway-count a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (not (seq a-seq)) (not (seq b-seq))) []
    (not (seq a-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (not (seq b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
    (if (or (not (seq a-seq)) (<= (count a-seq) 1))
      a-seq
      (let [[half1 half2] (halve a-seq)]
        (seq-merge (merge-sort half1) (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  (let [inits (inits a-seq)
        not-empty-inits (filter not-empty inits)
        monotonic? (fn [coll] (or (apply <= coll) (apply >= coll)))
        monotonic-inits (filter monotonic? not-empty-inits)
        order-by-size-desc (fn [a b] (>= (count a) (count b)))
        ordered-not-empty-monotonic-inits (sort order-by-size-desc monotonic-inits)
        longest-monotonic-init (first ordered-not-empty-monotonic-inits)
        longest-monotonic-init-count (count longest-monotonic-init)]

    (if (not (seq longest-monotonic-init))
      []
      (cons longest-monotonic-init
            (split-into-monotonics (drop longest-monotonic-init-count a-seq))))))


(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

