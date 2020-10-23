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
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [[fst] a-seq
          rfilt (my-filter pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst rfilt)
        rfilt))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (and (= (first a-seq) (first b-seq))
               (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (let [deced (dec up-to)]
      (cons deced (my-range deced)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse
                (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat
               (tails a-seq)
               (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)]
      (my-frequencies-helper (assoc freqs head (inc (get freqs head 0)))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat
         (map (fn [[k n]] (repeat n k)) a-map)))

(defn my-take [n coll]
  (cond
    (== n 0) '()
    (empty? coll) coll
    :else
      (cons (first coll)
            (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [hc (int (/ (count a-seq) 2))]
    [(take hc a-seq) (drop hc a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq)
            (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq)
            (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [back-init-tails (reverse (map vector (inits a-seq) (tails a-seq)))
          [mono rests] (first (my-drop-while (fn [[a]] ((complement monotonic?) a))
                                             back-init-tails))]
      (cons mono (split-into-monotonics rests)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [sset (set a-set)
          pperms (map (fn [e]
                        (map (fn [c] (cons e c))
                             (permutations (disj sset e))))
                      a-set)]
      (apply concat pperms))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [head (first a-set)
          prest (powerset (rest a-set))
          head-added (map (fn [s] (conj s head)) prest)]
      (set (concat prest head-added)))))

