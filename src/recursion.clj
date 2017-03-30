(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [count1 (count seq-1)
        count2 (count seq-2)]
    (if (> count1 count2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (cons first-elem (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (cons first-elem (my-take-while pred? (rest a-seq)))
      :else [])))

(defn my-drop-while [pred? a-seq]
  (let [first-elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? first-elem) (my-drop-while pred? (rest a-seq))
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [seq-1 seq-2]
  (let [new-rotation (concat seq-2 seq-1)]
    (if (empty? seq-2)
      '()
      (cons new-rotation (rotations-helper (concat seq-1 [(first seq-2)]) (rest seq-2))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ['()]
    (rotations-helper '() a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [first-value (first a-seq)
        freq-first-temp (get freqs first-value)
        freq-first (cond
                     (nil? first-value) -1
                     (nil? freq-first-temp) 0
                     :else freq-first-temp)]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs first-value (inc freq-first)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [first-elem (first a-map)]
    (if (empty? a-map)
      '()
      (concat (repeat (get first-elem 1) (get first-elem 0)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [index (int (/ (count a-seq) 2))]
    (concat [(my-take index a-seq)] [(my-drop index a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [both-empty (and (empty? a-seq) (empty? b-seq))
        decr-a-seq (cond
                     (empty? b-seq) true
                     (empty? a-seq) false
                     :else (< (first a-seq) (first b-seq)))
        next-elem (cond
                    both-empty nil
                    (empty? a-seq) (first b-seq)
                    (empty? b-seq) (first a-seq)
                    :else (min (first a-seq) (first b-seq)))
        new-a-seq (cond
                    (empty? a-seq) a-seq
                    decr-a-seq (rest a-seq)
                    :else a-seq)
        new-b-seq (cond
                    (empty? b-seq) b-seq
                    decr-a-seq b-seq
                    :else (rest b-seq))]
    (if both-empty
      []
      (cons next-elem (seq-merge new-a-seq new-b-seq)))))

(defn merge-sort [a-seq]
  (let [halved-seq (halve a-seq)
        half-1 (nth halved-seq 0)
        half-2 (nth halved-seq 1)]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort half-1) (merge-sort half-2)))))

(defn longest-increasing-seq-from-start [a-seq]
  (cond
    (or (empty? a-seq) (singleton? a-seq)) a-seq
    (> (nth a-seq 0) (nth a-seq 1)) [(nth a-seq 0)]
    :else (cons (nth a-seq 0) (longest-increasing-seq-from-start (rest a-seq)))))

(defn longest-decreasing-seq-from-start [a-seq]
  (cond
    (or (empty? a-seq) (singleton? a-seq)) a-seq
    (< (nth a-seq 0) (nth a-seq 1)) [(nth a-seq 0)]
    :else (cons (nth a-seq 0) (longest-decreasing-seq-from-start (rest a-seq)))))

(defn split-into-monotonics
  "test"
  [a-seq]
  (let [inc-seq (longest-increasing-seq-from-start a-seq)
        dec-seq (longest-decreasing-seq-from-start a-seq)
        inc-seq-len (count inc-seq)
        dec-seq-len (count dec-seq)]
  (cond
    (empty? a-seq) '()
    (> inc-seq-len dec-seq-len) (cons inc-seq (split-into-monotonics (drop inc-seq-len a-seq)))
    :else (cons dec-seq (split-into-monotonics (drop dec-seq-len a-seq))))))

(defn drop-nth-element [n a-seq]
  (concat (take n a-seq) (drop (inc n) a-seq)))

(defn element-to-rest-of-seq
  "Creates map (actually list of two element sequences) that maps an element to the rest of the sequence."
  [a-seq n seq-len]
  (if (= n seq-len)
    '()
    (cons [(nth a-seq n) (drop-nth-element n a-seq)]
          (element-to-rest-of-seq a-seq (inc n) seq-len))))

(defn concat-elem-to-seqs
  "Returns list of sequences where the element given is put at the start of each of the given sequences"
  [elem seqs]
  (if (empty? seqs)
    '()
    (cons (cons elem (first seqs)) (concat-elem-to-seqs elem (rest seqs)))))

(declare permutations-seq)
(declare permutations-helper)

(defn permutations-helper-loop [elem-to-seqs]
  "Loops through the result of the function element-to-rest-of-seq and creates permutations"
  (let [first-elem-to-seq (first elem-to-seqs)
        first-elem (get first-elem-to-seq 0)
        first-seq (get first-elem-to-seq 1)]
    (if (empty? elem-to-seqs)
      '()
      (concat (permutations-helper first-elem first-seq) (permutations-helper-loop (rest elem-to-seqs))))))

(defn permutations-helper [first-elem rest-seq]
  "Creates all permutations where first-elem is the first element"
    (concat-elem-to-seqs first-elem (permutations-seq rest-seq)))

(defn permutations-seq [a-seq]
  (let [elem-to-seqs (element-to-rest-of-seq a-seq 0 (count a-seq))]
    (cond
      (empty? a-seq) [()]
      (singleton? a-seq) [a-seq]
      :else (permutations-helper-loop elem-to-seqs))))

(defn permutations [a-set]
  (permutations-seq (seq a-set)))

(defn powerset [a-set]
  [:-])

