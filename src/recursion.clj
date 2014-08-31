(ns recursion)

(defn product [coll]
 (if (empty? coll)
   1
   (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
 (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
 (if (empty? coll)
   nil
   (if (empty? (rest coll))
     (first coll)
     (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
        (first a-seq)
        (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

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
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (my-map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)]
      (if (contains? freqs key)
        (my-frequencies-helper
          (assoc freqs key (inc (get freqs key)))
          (rest a-seq))
        (my-frequencies-helper
          (assoc freqs key 1)
          (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [index (int (/ (count a-seq) 2))]
    (vector (my-take index a-seq) (my-drop index a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a-first (first a-seq)
        b-first (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq))
        '()
      (empty? a-seq)
        b-seq
      (empty? b-seq)
        a-seq
      (< a-first b-first)
        (cons a-first (seq-merge (rest a-seq) b-seq))
      :else
        (cons b-first (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)
      '()
    (singleton? a-seq)
      a-seq
    :else
      (let [[first-part second-part] (halve a-seq)]
        (seq-merge
          (merge-sort first-part)
          (merge-sort second-part)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn monotonic-init [a-seq]
  (last (take-while monotonic? (drop 2 (reverse (inits a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono-split (monotonic-init a-seq)]
      (cons mono-split
            (split-into-monotonics (drop
                                   (count mono-split) a-seq))))))

(defn permutations-helper [work-items results]
  (cond
    (= (count work-items) 0)
      (seq (set results))
    :else
      (let [[prefix-seq rest-seq] (first work-items)
            rest-work-items       (rest work-items)
            rots                  (rotations rest-seq)
            concat-fn             (fn [elem] (concat prefix-seq elem))
            to-add                (map concat-fn rots)
            index                 (inc (count prefix-seq))
            splitter              (fn [elem] (split-at index elem))
            map-new-items         (map splitter to-add)
            filter-pred           (fn [elem] (>= (count (get elem 1)) 2))
            new-items             (filter filter-pred map-new-items)
            new-work-items        (concat rest-work-items new-items)
            new-results           (concat results to-add)]
        (permutations-helper new-work-items new-results))))

(defn permutations [a-set]
  (let [rots                  (rotations a-set)
        splitter              (fn [elem] (split-at 1 elem))
        work-items            (map splitter rots)]
    (permutations-helper work-items '())))

(defn powerset [a-set]
  [:-])
