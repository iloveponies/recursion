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

(defn my-last2 [coll]
  (cond
    (empty? (rest coll)) (first coll)
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
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons
            (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

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
    '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reverse-inits (tails (reverse a-seq))]
    (reverse (map reverse reverse-inits))))

(defn rotation [n a-seq]
  (if (zero? n)
    a-seq
    (rotation (dec n) (concat (rest a-seq) (list (first a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (let [number-of-rotations (count a-seq)]
      (map (fn [n] (rotation n a-seq)) (range number-of-rotations)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          new-freqs (if (contains? freqs element)
                     (assoc freqs element (inc (get freqs element)))
                     (assoc freqs element 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq freqs]
  (if (empty? freqs)
    a-seq
    (let [[element times] (first freqs)
           new-seq (concat a-seq (repeat times element))]
      (un-frequencies-helper new-seq (rest freqs)))))


(defn un-frequencies [a-map]
  (un-frequencies-helper (list) a-map))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-point (/ (count a-seq) 2)]
    [(my-take half-point a-seq) (my-drop half-point a-seq)]))

(defn seq-merge-helper [sorted-seq a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) sorted-seq
    (empty? a-seq) (concat sorted-seq b-seq)
    (empty? b-seq) (concat sorted-seq a-seq)
    :else (let [a (first a-seq) b (first b-seq)]
            (if (<= a b)
              (seq-merge-helper (conj sorted-seq a) (rest a-seq) b-seq)
              (seq-merge-helper (conj sorted-seq b) a-seq (rest b-seq))))))

(defn seq-merge
  "Takes two (low to high) sorted number sequences and combines them into one sorted sequence."
  [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-half second-half] (halve a-seq)]
        (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    nil
    (let [monotonics (take-while monotonic? (rest (inits a-seq)))
          longest-monotonic (first (reverse monotonics))]
        (cons longest-monotonic (split-into-monotonics (drop (count longest-monotonic) a-seq))))))

;(defn permutation-helper [permutations set-vector])

(defn permute-pair
  "Gives back a vector or permutations for a sequence of two items.
   E.G. #{:ver :len} => [[:vers :len][:len :vers]]."
  [pair]
  (let [a (first pair)
        b (second pair)]
    [[a b] [b a]]))

(defn permutations-helper [a-set]
  (cond
    (empty? a-set) [[]]
    (singleton? a-set) [[(first a-set)]]
    (= (count a-set) 2) (permute-pair a-set)
    :else (let [sub-permutations (fn [element]
                                   (map
                                     (fn [subset] (cons element subset))
                                     (permutations-helper (disj a-set element))))]
            (apply concat (map sub-permutations a-set)))))

(defn permutations [a-set]
  ;let's make sure we have a set
  (permutations-helper (set a-set)))

(defn powerset-helper [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [subsets (map (fn [element] (disj a-set element)) a-set)
          sub-powersets (apply clojure.set/union (map powerset-helper subsets))]
      (conj sub-powersets a-set))))

(defn powerset [a-set]
  ;Since we're using set union later on, we have to make sure we have a set.
  ;(The test suite is calling powerset with a vector.)
  (powerset-helper (set a-set)))
