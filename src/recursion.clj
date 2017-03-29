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

(defn tails-old [a-seq]
  (if (or (empty? a-seq) (and (singleton? a-seq) (empty? (first a-seq))))
    ['()]
    (cons a-seq (tails (rest a-seq)))))

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

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

