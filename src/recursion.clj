(ns recursion)

(defn product [coll]
  (if
    (empty? coll) 1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (and
   ((complement empty?) coll)
   (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max
          (first a-seq)
          (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max
          (first a-seq)
          (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [first-of-a-seq (first a-seq) rest-of-a-seq (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? first-of-a-seq) (cons first-of-a-seq (my-filter pred? rest-of-a-seq))
     :else (my-filter pred? rest-of-a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [first-of-a-seq (first a-seq)]
  (cond
   (empty? a-seq) a-seq
   (pred? first-of-a-seq) (cons first-of-a-seq (my-take-while pred? (rest a-seq)))
   :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or
    (not (= (count a-seq) (count b-seq)))
    (not (= (first a-seq) (first b-seq)))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if
    (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (<= how-many-times 0) '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (butlast a-seq)))))

(defn do-rotation [total-rotations a-seq]
  (if (zero? total-rotations)
    '()
    (let [seq-rotated (concat (rest a-seq) (cons (first a-seq) '()))]
      (cons
       a-seq
       (do-rotation (dec total-rotations) seq-rotated)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [total-rotations (count a-seq)]
      (do-rotation total-rotations a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [first-of-seq (first a-seq)
        rest-of-seq (rest a-seq)
        value-in-freq (fn [key'] (val (first (select-keys freqs [key']))))]
    (cond
     (empty? a-seq)
     freqs
     (contains? freqs first-of-seq)
     (my-frequencies-helper (conj freqs [first-of-seq (inc (value-in-freq first-of-seq))]) rest-of-seq)
     :else
     (my-frequencies-helper (conj freqs [first-of-seq 1]) rest-of-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [first-elem (first a-map)]
    (if (empty? a-map)
      '()
      (concat
       (repeat (int (val first-elem)) (key first-elem) )
       (un-frequencies (dissoc a-map (key first-elem)))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons
     (first coll)
     (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-seq (int (/ (count a-seq) 2))]
    (cons (my-take half-seq a-seq) (cons (my-drop half-seq a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (let [a-first (first a-seq) b-first (first b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< a-first b-first) (cons a-first (seq-merge (rest a-seq) b-seq))
     :else (cons b-first (seq-merge a-seq (rest b-seq))))))


(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [halve-seq (halve a-seq)]
      (seq-merge
       (merge-sort (first halve-seq))
       (merge-sort  (second halve-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

