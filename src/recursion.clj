(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

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
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (rest a-seq)]
      (if (pred? fst)
        (cons fst (my-filter pred? rst))
        (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [fst (first a-seq)]
      (if (pred? fst)
        (cons fst (my-take-while pred? (rest a-seq)))
        ()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
        a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (and (= (first a-seq)
                  (first b-seq))
               (seq= (rest a-seq)
                     (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (let [next (dec up-to)]
      (cons next (my-range next)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (seq [()])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (seq [()])
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)]
      (my-frequencies-helper
       (assoc freqs fst (inc (get freqs fst 0)))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[key val] (first a-map)]
      (concat (repeat val key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (== n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))
    
(defn halve [a-seq]
  (let [half-cnt (int (/ (count a-seq) 2))]
    [(my-take half-cnt a-seq) (my-drop half-cnt a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-fst (first a-seq)
                b-fst (first b-seq)]
            (if (< a-fst b-fst)
              (cons a-fst (seq-merge (rest a-seq) b-seq))
              (cons b-fst (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (singleton? a-seq) (seq a-seq)
    :else (let [[lo hi] (halve a-seq)]
            (seq-merge (merge-sort lo)
                       (merge-sort hi)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono-inits (take-while (fn [x] (or (empty? x)
                                             (singleton? x)
                                             (apply <= x)
                                             (apply >= x))) (inits a-seq))
          mono (first (drop (dec (count mono-inits)) mono-inits))]
      (cons mono (split-into-monotonics (drop (count mono) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    (seq ['()])
    (apply concat
     (map (fn [pivot]
            (let [a-subset (remove (fn [item] (= pivot item)) a-set)]
              (map (fn [p] (concat [pivot] p)) (permutations a-subset))))
          a-set))))

(defn powerset [a-set]
  (if (empty? a-set)
    (set [a-set])
    (let [fst (first a-set)
          pset (powerset (set (rest a-set)))]
      (set (concat (map (fn [x] (conj x fst)) pset) pset)))))
