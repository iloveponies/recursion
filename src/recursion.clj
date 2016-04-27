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
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

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
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      (seq a-seq)))

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
  (if
    (or (empty? seq-1) (empty? seq-2))
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
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (get freqs (first a-seq))
                      (assoc freqs (first a-seq)
                                   (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[elem n] (first a-map)]
      (concat (repeat n elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split (int (/ (count a-seq) 2))]
    (vector (my-take split a-seq) (my-drop split a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (<= (first a-seq) (first b-seq))
      (cons (first a-seq)
            (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq)
            (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [[first-seq second-seq] (halve a-seq)]
      (seq-merge
        (merge-sort first-seq)
        (merge-sort second-seq)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (cons
      (last (filter monotonic? (inits a-seq)))
      (split-into-monotonics
        (drop (count (last (filter monotonic? (inits a-seq)))) a-seq)))))

(defn size-two? [a-seq]
  (and (not (empty? a-seq)) (singleton? (rest a-seq))))

(defn permutations [a-set]
  (cond
    (empty? a-set)
      '(())
    (singleton? a-set)
      (seq a-set)
    (size-two? a-set)
      (list (seq a-set) (reverse a-set))
    :else
      (apply concat
             (map (fn [b-set]
               (map (fn [c-set] (cons (first b-set) c-set)) (permutations (rest b-set))))
          (rotations a-set)))))

(defn powerset-helper [a-set elem]
  (if (empty? a-set)
    #{}
    (set (apply list (set (cons elem (first a-set)))
                 (powerset-helper (rest a-set) elem)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (clojure.set/union (powerset (rest a-set))
                       (powerset-helper (powerset (rest a-set)) (first a-set)))))
