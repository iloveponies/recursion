(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (*
      (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) head
      :else (max head (max-element tail)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) head
      :else (seq-max head (longest-sequence tail)))))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) (empty a-seq)
      :else (if (pred? head)
              (cons head (my-filter pred? tail))
              (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)
        blank (empty a-seq)]
    (cond
      (empty? a-seq)
        blank
      (pred? head)
        (cons head (my-take-while pred? tail))
      :else
        blank)))

(defn my-drop-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)
        blank (empty a-seq)]
    (cond
      (empty? a-seq)
        blank
      (pred? head)
        (my-drop-while pred? tail)
      :else
        a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    :else (and
            (= (first a-seq) (first b-seq))
            (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or
        (empty? seq-1)
        (empty? seq-2))
    (empty seq-1)
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (< how-many-times 1)
    (empty ())
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    (empty ())
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (set (map concat (tails a-seq) (inits a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          freq (get freqs head)
          cnt (if freq freq 0)]
      (my-frequencies-helper
        (assoc freqs head (inc cnt))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (empty ())
    (let [entry (first a-map)
          k (first entry)
          v (second entry)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or
        (empty? coll)
        (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if (< n 1)
      (cons (first coll) (my-drop n (rest coll)))
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [e1 (empty? a-seq)
        e2 (empty? b-seq)]
    (cond
      e1 b-seq
      e2 a-seq
      :else (let [h1 (first a-seq)
                  h2 (first b-seq)]
              (if (< h1 h2)
                (cons h1 (seq-merge (rest a-seq) b-seq))
                (cons h2 (seq-merge a-seq (rest b-seq))))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)
          h1 (first halves)
          h2 (second halves)]
      (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn cons-end [a-seq elem]
  (reverse (cons elem (reverse a-seq))))

(defn split-while-mono [head tail]
  (if (empty? tail)
    [head tail]
    (let [new-head (cons-end head (first tail))]
      (if (monotonic? new-head)
        (split-while-mono new-head (rest tail))
        [head tail]))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    (empty ())
    (let [pair (split-while-mono () a-seq)
          head (first pair)
          tail (second pair)]
      (if (empty? tail)
        (seq [head])
        (cons head (split-into-monotonics tail))))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (cons head tail))))

(defn powerset [a-set]
  (if (empty? a-set)
    (list ())
    (let [subset (powerset (rest a-set))
          pair (fn [x] (conj x (first a-set)))]
      (clojure.set/union
        subset
        (map pair subset)))))

