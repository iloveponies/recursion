(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and
        (not (empty? coll))
        (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max-element (cons
                   (max (first a-seq) (first (rest a-seq)))
                   (rest (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
        seq-1
        seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (longest-sequence (cons
                        (seq-max (first a-seq) (first (rest a-seq)))
                        (rest (rest a-seq))))))

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
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
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
  (cond
    (or (empty? seq-1) (empty? seq-2))
      []
    :else
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
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse
                (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [temp
        (rest (map concat (tails a-seq) (inits a-seq)))]
    (if (empty? temp)
      '(())
      temp)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freq (if (contains? freqs (first a-seq))
                     (inc (get freqs (first a-seq)))
                     1)]
      (my-frequencies-helper
        (assoc freqs (first a-seq) new-freq)
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (val (first a-map)) (key (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (< 0 n)
      (my-drop (dec n) (rest coll))
      (seq coll))))

(defn halve [a-seq]
  (let [pivot (int (/ (count a-seq) 2))]
    (seq [(my-take pivot a-seq) (my-drop pivot a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      '()
    (empty? a-seq) (seq b-seq)
    (empty? b-seq) (seq a-seq)
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    (if (empty? a-seq)
      '()
      a-seq)
    (let [seq-1 (first (halve a-seq)) seq-2 (second (halve a-seq))]
      (seq-merge (merge-sort seq-1) (merge-sort seq-2)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or
      (apply <= a-seq)
      (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (monotonic? a-seq)
    (seq [a-seq])
    (let [mono-seqs (take-while monotonic? (inits a-seq))
          longest-mono (first (reverse mono-seqs))]
      (cons longest-mono (split-into-monotonics (drop (count longest-mono) a-seq))))))

(declare permutations-helper)
(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (seq [a-set])
    :else  (apply concat (map permutations-helper (rotations a-set)))))

(defn permutations-helper [a-seq]
  (map (fn [next-perms]
         (concat [(first a-seq)] next-perms))
       (permutations (rest a-seq))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [pset-rest (powerset (rest a-set))]
      (clojure.set/union pset-rest (map #(conj % (first a-set)) pset-rest)))))
