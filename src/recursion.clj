(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (boolean (and (seq coll) (empty? (rest coll)))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [h (first a-seq)
          t (rest a-seq)]
      (if (pred? h)
        (cons h (my-filter pred? t))
        (my-filter pred? t)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [h (first a-seq)
          t (rest a-seq)]
      (if (pred? h)
        (cons h (my-take-while pred? t))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [h (first a-seq)
          t (rest a-seq)]
      (if (pred? h)
        (my-drop-while pred? t)
        a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= 0 n) 0
    (= 0 k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (or (= 0 n) (= 1 n)) n
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (not (pos? how-many-times))
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          freq-val (get freqs el 0)]
      (my-frequencies-helper (assoc freqs el (inc freq-val)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [[el n] (first a-map)]
      (concat (repeat n el) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-at (int (/ (count a-seq) 2))
        first-halve (my-take split-at a-seq)
        rest-halve (my-drop split-at a-seq)]
    [first-halve rest-halve]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [[first-halve rest-halve] (halve a-seq)]
      (seq-merge (merge-sort first-halve) (merge-sort rest-halve)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonic? (fn [xs]
                      (or (apply <= xs)
                          (apply >= xs)))
         inits (rest (reverse (inits a-seq)))
         biggest-monotonic (first (reverse (take-while monotonic? inits)))]
     (cons biggest-monotonic (split-into-monotonics (drop (count biggest-monotonic) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

