(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (let [head (first coll)
        tail (rest coll)]
    (if (empty? tail)
      head
      (my-last tail))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
      (if (pred? head)
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        :else (and (= (first a-seq) (first b-seq))
                   (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (== n 0) 0
        (== k 0) 1
        :else (* n (power n (- k 1)))))

(defn fib [n]
  (if (or (== n 0) (== n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (let [cur (- up-to 1)]
      (cons cur (my-range cur)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [rev (tails (reverse a-seq))]
    (map reverse rev)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [t (tails a-seq)
          i (reverse (inits a-seq))]
      (rest (map concat t i)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          new-freqs (assoc freqs head (inc (freqs head 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))
        front (my-take mid a-seq)
        back (my-drop mid a-seq)]
    [front back]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq))
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [[left right] (halve a-seq)
          lsort (merge-sort left)
          rsort (merge-sort right)]
      (seq-merge lsort rsort))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a-seq-inits (rest (reverse (inits a-seq)))
          monotonic (last (take-while monotonic? a-seq-inits))
          remaining (drop (count monotonic) a-seq)]
      (cons monotonic (split-into-monotonics remaining)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat
           (map (fn [x]
                  (map (fn [y]
                         (cons x y))
                       (permutations (remove #{x} a-set))))
                a-set))))

(defn powerset [a-set]
  (if (not (set? a-set))
    (powerset (set a-set))
    (apply clojure.set/union
           #{a-set}
           (map (fn [x] (powerset (disj a-set x))) a-set))))
