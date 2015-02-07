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
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

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
    (zero? n) 0
    (== n 1) 1
    :else (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [next (dec up-to)]
    (if (zero? up-to)
      '()
      (cons next (my-range next)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (let [rotate (fn [s] (concat (rest s) [(first s)]))]
      (rest (reduce (fn [memo, v]
                      (conj memo (rotate (last memo))))
                    (vector a-seq) a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)
          new-freqs (if (contains? freqs first-element)
                      (update-in freqs [first-element] inc)
                      (assoc freqs first-element 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [first-elem (first a-map)
          what (first first-elem)
          how-many (second first-elem)]
      (concat (repeat how-many what) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (zero? n) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-count (int (/ (count a-seq) 2))]
    (vector (my-take half-count a-seq)
            (my-drop half-count a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< first-a first-b) (cons first-a (seq-merge (rest a-seq) b-seq))
      :else (cons first-b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[f, s] (halve a-seq)]
      (seq-merge (merge-sort f) (merge-sort s)))))

(defn split-into-monotonics [a-seq]
  (let [revinits (reverse (inits a-seq))
        pred (fn [x] (or (apply < x) (apply > x)))
        first-monotonic (last (take-while pred (drop 2 revinits)))]
    (if (<= (count a-seq) 1)
      a-seq
      (cons first-monotonic
            (split-into-monotonics (drop (count first-monotonic) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (reduce
     (fn [memo, v]
       (concat
        (let [sub (disj (set a-set) v)]
          (if (<= (count sub) 2)
            (map #(cons v %) (rotations sub))
            (map #(cons v %) (permutations sub))))
        memo))
     '()
     a-set)))

(defn powerset [a-set]
  (set
   (map set
        (apply concat
               (map inits
                    (permutations a-set))))))
