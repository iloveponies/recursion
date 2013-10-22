(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
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
  (if (> (count seq-1)(count seq-2))
    seq-1
	seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst) (cons fst rst) rst))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= (first a-seq) elem)
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (seq a-seq))))

(defn seq= [a-seq b-seq]
  (if (not (= (count a-seq) (count b-seq)))
    false
    (if (and (empty? a-seq) (empty? b-seq))
      true
      (and (= (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper [n a-seq]
  (if (= n (count a-seq))
    ()
    (cons
     (concat (drop n a-seq) (take n a-seq))
     (rotations-helper (inc n) a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
	(rotations-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (assoc freqs (first a-seq)
       (inc (get freqs (first a-seq) 0)))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat
     (repeat (first (vals a-map)) (first (keys a-map)))
     (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) coll
   (<= n 0) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    [(my-take midpoint a-seq)
     (my-drop midpoint a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (<= (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left)
                 (merge-sort right)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [next-monotonic (last (take-while monotonic? (drop 1 (inits a-seq))))]
      (cons next-monotonic (split-into-monotonics (drop (count next-monotonic) a-seq))))))

(defn permutations-helper [stone n a-set]
  (if (empty? a-set)
    (conj nil stone)
    (let [permutation (permutations-helper (conj stone (nth a-set n)) 0
                                           (concat (take n a-set) (drop (inc n) a-set)))]
      (if (= n (- (count a-set) 1))
	    permutation
        (concat permutation(permutations-helper stone (inc n) a-set))))))

(defn permutations [a-set]
  (permutations-helper () 0 (seq a-set)))

(defn powerset-helper [a-set n]
  (if (or (empty? a-set) (= n (count a-set)))
    ()
    (let [next-set (set (concat (take n a-set) (drop (inc n) a-set)))]
      (into (conj (powerset-helper next-set 0) next-set)
            (powerset-helper a-set (inc n))))))

(defn powerset [a-set]
  (conj (set (powerset-helper a-set 0)) (set a-set)))

