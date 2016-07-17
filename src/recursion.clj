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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn single-pred-seq [pred a-seq]
  (let [fst (first a-seq)]
    (if (or (empty? a-seq) (singleton? a-seq))
      fst
      (pred fst (single-pred-seq pred (rest a-seq))))))

(defn max-element [a-seq]
  (single-pred-seq max a-seq))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (single-pred-seq seq-max a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (rest a-seq)]
      (if (pred? fst)
        (cons fst (my-filter pred? rst))
        (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)]
    (if (or (empty? a-seq) (not (pred? fst)))
      ()
      (cons fst (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [aemp (empty? a-seq)
        bemp (empty? b-seq)]
    (cond (and aemp bemp) true
          (or (and aemp (not bemp)) (and (not aemp) bemp)) false
          (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
          :else false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (== n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (let [n (dec up-to)] (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotate [a-seq]
  (if (empty? a-seq)
    ()
    (cons (last a-seq) (butlast a-seq))))

(defn rotations
  ([vals] (rotations vals (rotate vals) ()))
  ([vals rvals acc]
     (if (= vals rvals)
       (cons vals acc)
       (recur vals (rotate rvals) (cons rvals acc)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)]
      (my-frequencies-helper
       (assoc freqs fst (if (contains? freqs fst)
                          (inc (get freqs fst))
                          1))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [fst (first a-map)
          key (first fst)
          cnt (second fst)]
      (concat (repeat cnt key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (zero? n) coll
        (empty? coll) ()
        :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    ()
    (let [c (int (/ (count a-seq) 2))]
      (cons (my-take c a-seq) (cons (my-drop c a-seq) ())))))

(defn seq-merge [a-seq b-seq]
  (let [aemp (empty? a-seq)
        bemp (empty? b-seq)
        fsa (first a-seq)
        fsb (first b-seq)]
    (cond (and aemp bemp) ()
          (and aemp (not bemp)) b-seq
          (and (not aemp) bemp) a-seq
          (= fsa (min fsa fsb)) (cons fsa (seq-merge (rest a-seq) b-seq))
          :else (cons fsb (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [half (halve a-seq)
          fh (first half)
          sh (second half)]
      (seq-merge (merge-sort fh) (merge-sort sh)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

