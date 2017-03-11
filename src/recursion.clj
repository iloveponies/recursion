(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
          (first coll)
          (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (or (max-element (rest a-seq)) (first a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (or (and (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (or (and (not (empty? a-seq)) (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))) '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    '()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
    (seq a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (and (not (empty? a-seq)) (not (empty? b-seq)) (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq)
    '(())
    (not (= (last a-seq) '()))
    (rotations (cons (seq a-seq) (cons '() '())))
    (= (count (rest a-seq)) (+ 1 (count (first a-seq))))
    '()
    :else
    (cons (first a-seq) (rotations (cons (concat (rest (first a-seq)) [(first (first a-seq))]) (cons '() (rest a-seq)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [nfreqs (assoc freqs (first a-seq) (inc (or (get freqs (first a-seq)) 0)))]
      (my-frequencies-helper nfreqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [repn (last (first a-map))
        repv (first (first a-map))]
    (if (empty? a-map)
      nil
      (concat (repeat repn repv) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (>= n (count coll))
    '()
    (reverse (my-take (- (count coll) n) (reverse coll)))))

(defn halve [a-seq]
  (let [ffn (int (/ (count a-seq) 2))]
    (vector (my-take ffn a-seq) (my-drop ffn a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
    b-seq
    (and (not (empty? b-seq)) (> (first a-seq) (first b-seq)))
    (seq-merge (concat a-seq [(first b-seq)]) (rest b-seq))
    :else
    (seq-merge (rest a-seq) (concat [(first a-seq)] b-seq))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    (or (and (empty? a-seq) '()) a-seq)
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [crease (if (< (first a-seq) (first (rest a-seq)))
                   <
                   >)
          pseq (take-while (fn [x] (crease (first x) (last x))) (map (fn [x y] [x y]) a-seq (rest a-seq)))
          monotonic (concat (map first pseq) [(last (last pseq))])]
      (cons monotonic (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

