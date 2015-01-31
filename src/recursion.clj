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
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
       (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [a-seq1 (drop-last a-seq)]
    (cond
     (empty? a-seq)
       a-seq
    (every? pred? a-seq)
      a-seq
     :else
       (my-take-while pred? (drop-last a-seq)))))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (not= (first a-seq) (first b-seq))
     false
   :else
     (seq= (rest a-seq) (rest b-seq))))

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
   (== n 0)
     0
   (== n 1)
     1
   :else
     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [up (dec up-to)]
  (if (neg? up)
    '()
    (cons up (my-range up)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (drop-last a-seq)))))

(defn rotate [b-seq n]
  (let [rotation (concat (rest b-seq) [(first b-seq)])]
  (if (zero? n)
    []
    (cons rotation (rotate rotation (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [b-seq (remove (fn [x] (= (first a-seq) x)) a-seq)
        nfreqs (assoc freqs (first a-seq) (- (count a-seq) (count b-seq)))]
    (if (empty? b-seq)
      nfreqs
      (my-frequencies-helper nfreqs b-seq))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies-helper [freqs a-seq]
  (let [n-seq (concat a-seq (repeat (second (first freqs)) (ffirst freqs)))]
    (if (empty? freqs)
      a-seq
      (un-frequencies-helper (rest freqs) n-seq))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map []))

(defn my-take-helper [n coll]
  (if (zero? n)
    coll
    (my-take-helper (dec n) (drop-last coll))))

(defn my-take [n coll]
  (my-take-helper (max (- (count coll) n) 0) coll))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        h1 (my-take half a-seq)
        h2 (my-drop half a-seq)]
    [h1 h2]))

(defn seq-merge-helper [a-seq b-seq n-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     n-seq
   (empty? a-seq)
     (cons (first b-seq) (seq-merge-helper a-seq (rest b-seq) n-seq))
   (empty? b-seq)
     (cons (first a-seq) (seq-merge-helper (rest a-seq) b-seq n-seq))
   (< (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge-helper (rest a-seq) b-seq n-seq))
   :else
     (cons (first b-seq) (seq-merge-helper a-seq (rest b-seq) n-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

(defn merge-sort [a-seq]
  (let [half-seq (halve a-seq)]
    (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort (first half-seq))
                 (merge-sort (second half-seq))))))

(defn split-into-monotonics [a-seq]
  (loop [b-seq a-seq
         c-seq a-seq
         n-seq []]
    (cond
     (empty? c-seq)
       n-seq
     (or (apply <= b-seq) (apply >= b-seq))
       (recur (my-drop (count b-seq) c-seq) (my-drop (count b-seq) c-seq) (conj n-seq b-seq))
     :else
       (recur (drop-last b-seq) c-seq n-seq))))


(defn collect-seqs [a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [b-seq a-seq]
      (if (not (every? seq? b-seq))
        b-seq
        (recur (apply concat b-seq))))))

(defn permutation-helper [a-seq start]
  (let [atstart (get a-seq start)]
    (if (== (count a-seq) (inc start))
      a-seq
      (for [x (range start (count a-seq))]
        (let [atx (get a-seq x)
              swapped (assoc-in (assoc-in a-seq [start] atx) [x] atstart)]
          (permutation-helper swapped (inc start)))))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (collect-seqs (permutation-helper (vec a-set) 0))))

(defn powerset-helper [a-set b-set]
  (if (empty? a-set)
    (set b-set)
    (set (reduce concat
            (for [x a-set]
              (powerset-helper (disj a-set x) (conj b-set (disj a-set x))))))))

(defn powerset [a-set]
  (conj (powerset-helper (set a-set) #{}) (set a-set)))
