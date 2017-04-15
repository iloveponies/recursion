(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (not(empty? a-seq)) (empty? b-seq)) false
   (and (not(empty? b-seq)) (empty? a-seq)) false
   (and (empty? a-seq) (empty? b-seq)) true
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   (f (first seq-1) (first seq-2)) (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   :else '()))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec (dec n))) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (inits (butlast a-seq)))))

(defn rotations-helper [a-seq b-seq]
  (if (empty? b-seq)
    '()
    (cons (concat b-seq a-seq)
          (rotations-helper (concat a-seq (seq [(first b-seq)])) (rest b-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper '() a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [eka (first a-seq)
          freq (if (nil? (freqs eka)) 0 (freqs eka))]
      (my-frequencies-helper (assoc freqs eka (inc freq))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[elem freq] (first a-map)]
      (concat (repeat freq elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
   (cond
    (or (<= n 0) (empty? coll)) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (> n 0) (my-drop (dec n) (rest coll))
   :else coll))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
   [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) a-seq
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [half (halve a-seq)]
       (seq-merge (merge-sort (first half)) (merge-sort (second half))))))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
   ()
    (let [s (first (filter monotonic? (inits a-seq)))]
      (cons s (split-into-monotonics (drop (count s) a-seq))))))

(defn permutations-helper [right left]
  (if (empty? left)
    (list right)
    (apply concat
           (map (fn [x] (permutations-helper
                         (cons x right)
                         (disj left x)))
                left))))

(defn permutations [a-set]
  (permutations-helper '() (into #{} a-set)))

(defn powerset-helper [right left]
  (if (empty? left)
    (list right)
    (concat (powerset-helper right (rest left))
            (powerset-helper (conj right (first left)) (rest left)))))

(defn powerset [a-set]
  (powerset-helper #{} a-set))

