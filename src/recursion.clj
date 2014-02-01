(ns recursion
  (:use clojure.set))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and 
    (not (empty? coll))
    (empty? (rest coll))))

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
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond 
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

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
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn- rec-rotations [a-seq n]
  (cond 
    (= 0 n) ['()]
    (= 1 n) [a-seq]
    :else (let [rotation (concat (rest a-seq) [(first a-seq)])] 
            (cons a-seq (rec-rotations rotation (- n 1))))))

(defn rotations [a-seq]
  (rec-rotations a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (merge-with + freqs {(first a-seq) 1}) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [current (first a-map)
          elem (key current)
          how-many (val current)]
      (concat (repeat how-many elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond 
    (empty? coll) '()
    (> n 0) (my-drop (- n 1) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [half-point (int (/ (count a-seq) 2))]
    [(my-take half-point a-seq) (my-drop half-point a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else 
    (let [first-a (first a-seq)
          first-b (first b-seq)]
      (if (< first-a first-b)
        (cons first-a (seq-merge (rest a-seq) b-seq))
        (cons first-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)] 
      (seq-merge (merge-sort (first halves)) (merge-sort (get halves 1))))))

(defn- monotonic-here? [f prev-elem a-seq]
  (if (empty? a-seq)
    false
    (or (nil? prev-elem) (f (first a-seq) prev-elem))))

(defn- length-of-monotonic-part [f length-so-far prev-elem a-seq]
  (if (monotonic-here? f prev-elem a-seq)
    (length-of-monotonic-part f (inc length-so-far) (first a-seq) (rest a-seq))
    length-so-far))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [increasing-part-length (length-of-monotonic-part >= 0 nil a-seq)
          decreasing-part-length (length-of-monotonic-part <= 0 nil a-seq)
          longest-part (max increasing-part-length decreasing-part-length)]
      (cons (take longest-part a-seq) 
            (split-into-monotonics (drop longest-part a-seq))))))

(defn- next-level-of-permutation-tree [current-level]
  (let [being-built (first current-level)
        remaining-items (last current-level)]
    (if (empty? remaining-items) 
      [[being-built remaining-items]]
      (for [x remaining-items]
        [(cons x being-built) (disj remaining-items x)]))))

(defn- permutations-helper [permutation-tree-leaves level-index end-level-index]
  (if (= level-index end-level-index)
    permutation-tree-leaves
    (permutations-helper 
      (mapcat next-level-of-permutation-tree permutation-tree-leaves) 
      (inc level-index) end-level-index)))

(defn permutations [a-set]
  (map first (permutations-helper [[[] (set a-set)]] 0 (count a-set))))

(defn- next-level-of-subsets [a-set]
  (set (for [x a-set]
    (disj a-set x))))

(defn- powerset-helper [powerset-tree-leaves level-index end-level-index]
  (let [leave-set (set powerset-tree-leaves)]
    (if (>= level-index end-level-index)
      leave-set
      (clojure.set/union leave-set (powerset-helper 
                                     (mapcat next-level-of-subsets leave-set) 
                                     (inc level-index) 
                                     end-level-index)))))

(defn powerset [a-set]
  (powerset-helper #{(set a-set)} 0 (count a-set)))
