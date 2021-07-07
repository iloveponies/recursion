(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (seq (rest coll))
    false
    (not (empty? coll))))

(singleton? [1 2])

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (if (> len-1 len-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (reduce
    seq-max
    nil
    a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a (first a-seq)
          fa (pred? a)]
      (if fa
        (cons a (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      (empty a-seq))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= 0 n) 0
    (= 0 k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (reduce #(assoc %1 %2 (if (contains? %1 %2)
                          (inc (get %1 %2))
                          1))
          freqs a-seq))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat #(repeat (second %)
                   (first %))
          a-map))

(defn my-take [n coll]
  (if (and (> n 0) (not-empty coll))
    (cons (first coll)
          (my-take (dec n) (rest coll)))
    '()))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n-half (int (/ (count a-seq) 2))]
    (vector (take n-half a-seq) (drop n-half a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a0 (first a-seq)
        b0 (first b-seq)]
    (cond
      (and (nil? a0) (nil? b0)) '()
      (nil? a0) b-seq
      (nil? b0) a-seq
      (< a0 b0) (cons a0 (seq-merge (rest a-seq) b-seq))
      :else (cons b0 (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn n-first-monotonic [a-seq]
  (let [forward-diff (map - a-seq (rest a-seq))
        n-acc (count (take-while neg? forward-diff))
        n-desc (count (take-while pos? forward-diff))]
    (inc (max n-acc n-desc))))
(defn split-into-monotonics [a-seq]
  (let [n (n-first-monotonic a-seq)] 
    (if (seq a-seq)
      (cons (take n a-seq)
            (split-into-monotonics (drop n a-seq))))))

(defn insert [a-set x]
  (let [coll (range (inc (count a-set)))]
    (map #(concat (take % a-set)
                  [x]
                  (drop % a-set))
         coll)))

(defn permutations [coll]
  (cond
    (empty? coll) (list '())
    (= (count coll) 1) (list (seq coll))
    :else (mapcat #(insert %1 %2) (permutations (rest coll)) (repeat (first coll)))))

(defn set-choice [a-set n]
  (if (or (= n 0) (empty? a-set))
    #{#{}}
    (clojure.set/union 
      (set-choice (rest a-set) n)
      (map #(set (cons (first a-set) %)) (set-choice (rest a-set) (dec n))))))

(defn powerset [a-set]
  (set-choice a-set (count a-set)))
