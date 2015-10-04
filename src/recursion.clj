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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
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
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))
   :else
     []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not= (count a-seq) (count b-seq))
     false
   (empty? a-seq)
     true
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
   (zero? n) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [a-tails (tails (reverse a-seq))]
    (reverse (map reverse a-tails))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-key (first a-seq)
          current-freq (if (contains? freqs a-key) (get freqs a-key) 0)
          new-freqs (assoc freqs a-key (inc current-freq))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[a-key a-num] (first a-map)]
      (concat (repeat a-num a-key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) []
   (zero? n) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   (<= (first a-seq)
       (first b-seq))
     (cons (first a-seq)
           (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq)
           (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half)
                 (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [monotonic? (fn [xs] (or (empty? xs)
                                  (apply <= xs)
                                  (apply >= xs)))
          seq-inits (inits a-seq)
          current-monotonic (last (take-while monotonic? seq-inits))
          current-monotonic-count (count current-monotonic)]
      (cons current-monotonic
            (split-into-monotonics (drop current-monotonic-count a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat
           (map (fn [rotation]
                  (map (fn [sub-perm]
                         (cons (first rotation)
                               sub-perm))
                       (permutations (rest rotation))))
                (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [sub-powerset (powerset (rest a-set))]
      (concat sub-powerset
              (map (fn [sub-set] (conj sub-set (first a-set)))
                   sub-powerset)))))






