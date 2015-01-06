(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (true? (and
          (seq coll)
          (empty? (rest coll)))))

(defn my-last [coll]
  (if (seq (rest coll))
    (my-last (rest coll))
    (first coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        (singleton? (rest a-seq)) (seq-max (first a-seq) (second a-seq))
        :else (longest-sequence (rest a-seq))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) []
    (and (singleton? a-seq) (pred? (first a-seq))) (first a-seq)
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (drop 1 a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat
               (tails a-seq)
               (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)
          new-f-value (inc (get freqs f 0))]
      (my-frequencies-helper (assoc freqs f new-f-value) (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll)
          (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)    b-seq
    (empty? b-seq)    a-seq
    (> (first a-seq)
       (first b-seq)) (cons (first b-seq)
                            (seq-merge a-seq (rest b-seq)))
       :else             (cons (first a-seq)
                               (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)     a-seq
    (singleton? a-seq) a-seq
    :else              (let [[a b] (halve a-seq)]
                         (seq-merge (merge-sort a)
                                    (merge-sort b)))))


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [is-monotonic? (fn [s] (or (empty? s) (apply <= s) (apply >= s)))
          longest (last (take-while is-monotonic? (inits a-seq)))]
      (cons longest (split-into-monotonics (drop (count longest) a-seq))))))

(defn permutations-helper [a-set]
  (if (empty? a-set)
    '(())
    (apply concat (map (fn [x]
                         (map (fn [y]
                                (concat (list x) y))
                              (permutations-helper (disj a-set x))))
                       a-set))))

(defn permutations [a-set]
  (permutations-helper (set a-set)))

(defn powerset-helper [a-set]
  (if (empty? a-set)
    #{#{}}
    (set (map set
              (apply concat (map (fn [x]
                                   (let [pset (powerset-helper
                                               (disj a-set x))]
                                     (concat (map (fn [y] (concat (list x) y))
                                                  pset)
                                             pset)))
                                 a-set))))))

(defn powerset [a-set]
  (powerset-helper (set a-set)))

