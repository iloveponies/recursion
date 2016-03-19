(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
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
    (let [first (first a-seq)
          rest (rest a-seq)]
      (if (pred? first)
        (cons first (my-filter pred? rest))
        (my-filter pred? rest)))))

(defn sequence-contains? [elem a-seq]
  (let [first (first a-seq)]
    (cond (empty? a-seq) false
          (= elem first) true
          :else (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [first (first a-seq)]
      (cond (not (pred? first)) '()
            :else (cons first (my-take-while pred? (rest a-seq)))))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (and (not (empty? a-seq)) (empty? b-seq)) false
        (and (empty? a-seq) (not (empty? b-seq))) false
        (not= (first a-seq) (first b-seq)) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (== 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (let [dec-up-to (dec up-to)]
      (conj (my-range dec-up-to) dec-up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse (seq a-seq))))))

(defn rotations [a-seq]
  (let [tails (tails a-seq)
        heads (inits a-seq)
        concatenated (map concat tails heads)]
    (seq (set (filter (fn [x] (== (count x) (count a-seq))) concatenated)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first (first a-seq)
          freq (if (contains? freqs first)
                 (inc (get freqs first))
                 1)]
      (my-frequencies-helper (assoc freqs first freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [entry] (let [elem (first entry)
                                       freq (second entry)]
                                   (repeat freq elem))) a-map)))

(defn my-take [n coll]
  (cond (zero? n) '()
        (empty? coll) '()
        :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (zero? n) (seq coll)
        (empty? coll) '()
        :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [count (count a-seq)
        first-half-count (int (/ count 2))]
    [(my-take first-half-count a-seq) (my-drop first-half-count a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [a-first (first a-seq)
                    pred? (fn [x] (<= x a-first))]
                (concat
                  (my-take-while pred? b-seq)
                  (cons a-first '())
                  (seq-merge (rest a-seq) (my-drop-while pred? b-seq))))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) '()
        (singleton? a-seq) (seq a-seq)
        :else (let [halves (halve a-seq)
                    first-half (first halves)
                    second-half (second halves)]
                (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

