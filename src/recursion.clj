(ns recursion)

(defn product [coll]
  (cond (empty? coll) 1 :else (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond (> (count seq-1) (count seq-2)) seq-1
        :else seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (and
        (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (let [current (- up-to 1)]
    (if (> 0 current)
      []
      (cons current (my-range current)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits-helper [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (conj (inits-helper (rest a-seq)) (reverse a-seq))))

(defn inits [a-seq]
  (inits-helper (reverse a-seq)))

(defn rotations-helper [a-seq rotations]
  (if (= (count a-seq) (count rotations))
    rotations
    (rotations-helper (concat (rest a-seq) (conj [] (first a-seq))) (cons a-seq rotations))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-helper a-seq [])))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-count (if (contains? freqs elem)
                  (inc (get freqs elem))
                  1)]
      (my-frequencies-helper (assoc freqs elem elem-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [freq->vector
        (fn [[elem freq]] (repeat freq elem))]
    (if (empty? a-map)
    []
    (concat (freq->vector (first a-map)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (<= (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[halve-1 halve-2] (halve a-seq)]
      (seq-merge (merge-sort halve-1) (merge-sort halve-2)))))

(defn dir-fn [dir val]
  (fn [x] (dir val x)))

(defn mon-helper [dir prev a-seq]
  (if (empty? a-seq)
    '()
    (let [elem (first a-seq)]
      (if (dir prev elem)
        (concat [elem] (mon-helper dir elem (rest a-seq)))
        '()))))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [elem (first a-seq)
          a-seq-tail (rest a-seq)
          rising (cons elem (mon-helper < elem a-seq-tail))
          falling (cons elem (mon-helper > elem a-seq-tail))]
      (if (singleton? rising)
        (cons falling (split-into-monotonics (drop (count falling) a-seq)))
        (cons rising (split-into-monotonics (drop (count rising) a-seq)))))))

(defn flatten-levels [levels a-seq]
  (if (zero? levels)
    a-seq
    (flatten-levels (dec levels) (apply concat a-seq))))

(defn perm-helper [perm a-set]
  (if (empty? a-set)
    perm
    (map (fn [a] (perm-helper (conj perm a) (remove #{a} a-set))) a-set)))

(defn permutations [a-set]
  (if (empty? a-set)
    '([])
    (flatten-levels (dec (count a-set)) (perm-helper [] a-set))))

(defn powerset [a-set]
  [:-])
