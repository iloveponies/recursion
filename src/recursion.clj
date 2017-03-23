(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    (not= elem (first a-seq))
    (sequence-contains? elem (rest a-seq))
    :else
    false))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
    ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    ()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    (cons (first a-seq) (my-drop-while (fn [x] false) (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (or (empty? a-seq) (empty? b-seq))
    false
    (not= (first a-seq) (first b-seq))
    false
    :else
    (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (empty? seq-2)
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [r-seq (reverse a-seq)]
    (reverse (map reverse (tails r-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper freqs (rest a-seq))
      (my-frequencies-helper
       (assoc freqs (first a-seq) (count (filter (fn [x] (= (first a-seq) x)) a-seq))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (apply repeat (reverse (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (>= n (count coll))
    (seq coll)
    (when (< 0 n)
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (>= 0 n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-num (int (/ (count a-seq) 2))]
    (split-at split-num a-seq)))

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (concat a-seq b-seq)
    (if (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge (merge-sort (get (halve a-seq) 0)) (merge-sort (get (halve a-seq) 1)))))

(defn monotonics? [a-seq]
  (if (empty? a-seq)
    false
    (or (apply >= a-seq)
        (apply <= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [b-seq (first (reverse (take-while monotonics? (rest (inits a-seq)))))]
      (cons b-seq (split-into-monotonics (drop (count b-seq) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    [()]
    (apply concat (map
                   (fn [b-set] (map cons (repeat (first b-set))
                                    (permutations (rest b-set))))
                   (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{()}
    (let [s (powerset (rest a-set))]
      (clojure.set/union s (map #(conj % (first a-set)) s)))))

