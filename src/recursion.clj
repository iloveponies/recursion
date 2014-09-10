(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (== (count coll) 1))

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
      (let [r (max-element (rest a-seq))
            f (first a-seq)]
        (if (> f r)
          f
          r)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          r (my-filter pred? (rest a-seq))]
      (if (pred? f)
        (cons f r)
        r))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)]
      (if (pred? f)
        (cons f (my-take-while pred? (rest a-seq)))
        (empty a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)]
      (if (pred? f)
        (my-drop-while pred? (rest a-seq))
        (cons f (rest a-seq))))))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (not (== (count a-seq) (count b-seq)))
      false
      (if (not (== (first a-seq) (first b-seq)))
        false
        (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (empty seq-1)
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (if (== (mod k 2) 0)
      (let [a (power n (/ k 2))]
        (* a a))
      (* n (power n (- k 1))))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [n a]
  (if (<= n 0)
    ()
    (cons a (my-repeat (- n 1) a))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (let [n (dec up-to)]
    (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq (empty a-seq))
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [a-seq to-rotate n]
  (if (== n (count to-rotate))
    a-seq
    (let [rotated (concat (rest to-rotate) [(first to-rotate)])] 
      (cons rotated (rotate a-seq rotated (inc n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons (empty a-seq) a-seq)
    (rotate () a-seq 0)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)]
      (my-frequencies-helper (if (contains? freqs f)
                               (assoc freqs f (inc (get freqs f)))
                               (assoc freqs f 1))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [x (first a-map)
          k (first x)
          v (last x)]
      (concat (my-repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if (zero? n)
      (seq coll)
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [n (count a-seq)
        k (int (/ n 2))]
    [(my-take k a-seq) (my-drop k a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (concat a-seq b-seq)
    (let [a (first a-seq)
          b (first b-seq)]
      (if (< a b)
        (cons a (seq-merge (rest a-seq) b-seq))
        (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    ()
    (if (== (count a-seq) 1)
      a-seq
      (let [halves (halve a-seq)]
        (seq-merge (merge-sort (first halves)) (merge-sort (second halves)))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

