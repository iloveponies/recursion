(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (let [end (rest coll)]
    (if (empty? end) (first coll) (my-last end))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)
        lst (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? fst)
        (cons fst (my-filter pred? lst))
        (my-filter pred? lst)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)
        lst (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? fst)
        (cons fst (my-take-while pred? lst))
        '()))))

(defn my-drop-while [pred? a-seq]
  (let [fst (first a-seq)
        lst (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? fst)
        (my-drop-while pred? lst)
        a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq) (count b-seq)) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs (first a-seq) (inc (or (get freqs (first a-seq)) 0))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [key-1 (first (keys a-map))
          val-1 (first (vals a-map))]
    (concat (repeat val-1 key-1) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (= n 0) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge-helper [coll a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) coll
    (empty? a-seq) (concat coll b-seq)
    (empty? b-seq) (concat coll a-seq)
    :else (if (<= (first a-seq) (first b-seq))
            (seq-merge-helper (conj coll (first a-seq)) (rest a-seq) b-seq)
            (seq-merge-helper (conj coll (first b-seq)) a-seq (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

