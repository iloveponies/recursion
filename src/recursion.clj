(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else '()))

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
  (if (or (empty? seq-1) (empty? seq-2))
    '()
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
  (if (< 0 how-many-times)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (let [number (dec up-to)]
    (if (< 0 up-to)
      (cons number (my-range number))
      '())))

(defn tails [a-seq]
  (if (empty? a-seq)
     '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
     '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (my-map concat (rest (reverse (tails a-seq))) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-item (first a-seq)
          cur-value (get freqs first-item)
          new-freqs (if (= cur-value nil)
                      (assoc freqs first-item 1)
                      (assoc freqs first-item (inc cur-value)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[item repeats] (first a-map)]
      (concat (repeat repeats item) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (conj (my-take (- n 1) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-first (first a-seq) b-first (first b-seq)]
      (if (< a-first b-first)
        (conj (seq-merge (rest a-seq) b-seq) a-first)
        (conj (seq-merge (rest b-seq) a-seq) b-first)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a-halve b-halve] (halve a-seq)
          a-sorted (merge-sort a-halve)
          b-sorted (merge-sort b-halve)]
      (seq-merge a-sorted b-sorted))))


(defn monotonics-helper [a-seq]
  (if (empty? a-seq)
    '()
    (cons (take 2 a-seq) (monotonics-helper (drop 2 a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (= (mod (count a-seq) 2) 1)
    (cons (take 3 a-seq) (monotonics-helper (drop 3 a-seq)))
    (cons (take 2 a-seq) (monotonics-helper (drop 2 a-seq)))))

(defn my-interleave [a-number a-set]
  (if (empty? a-set)
    [[a-number]]
    (let [add-number (fn [b-set] (cons (first a-set) b-set))]
      (concat [(cons a-number a-set)]
            (map add-number (my-interleave a-number (rest a-set)))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    [a-set]
    (apply concat (map (fn [b-set] (my-interleave (first a-set) b-set))
         (permutations (rest a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    [a-set]
    (let [subsets (powerset (rest a-set))]
      (concat subsets (map (fn [b-set] (cons (first a-set) b-set))
                           subsets)))))

