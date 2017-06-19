(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
    '()
    (let [a (first a-seq)
          rest (rest a-seq)]
      (if (empty? rest)
        (if (pred? a)
          (cons a '())
          '())
        (if (pred? a)
          (cons a (my-filter pred? rest))
          (my-filter pred? rest))))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    (= how-many-times 1) (cons what-to-repeat '())
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= up-to 0) '()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (= (count a-seq) 0)
    (vector '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (= (count a-seq) 0)
    (vector '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rot-helper [a-seq n]
  (let [rot (concat (rest a-seq) (cons (first a-seq) '()))]
    (if (= n 1)
      (cons rot '())
      (cons rot (rot-helper rot (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (vector '())
    (rot-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (un-frequencies-helper (concat a-seq (repeat (get (first a-map) 1) (get (first a-map) 0))) (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (concat (vector (first coll)) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (== n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (cons (my-take h a-seq) (vector (my-drop h a-seq)))))

(defn seq-merge-helper [res a-seq b-seq]
  (if (empty? a-seq)
    (concat res b-seq)
    (if (empty? b-seq)
      (concat res a-seq)
      (if (< (first a-seq) (first b-seq))
        (seq-merge-helper (concat res (vector (first a-seq))) (rest a-seq) b-seq)
        (seq-merge-helper (concat res (vector (first b-seq))) a-seq (rest b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn mono-helper [a-seq b-seq result]
  (if (empty? a-seq)
    (if (empty? b-seq)
      result
      (mono-helper (concat a-seq (vector (first b-seq))) (rest b-seq) result))
    (if (empty? b-seq)
      (conj result a-seq)
      (if (monotonic? (concat a-seq (vector (first b-seq))))
        (mono-helper (concat a-seq (vector (first b-seq))) (rest b-seq) result)
        (mono-helper (vector (first b-seq)) (rest b-seq) (conj result a-seq))))))

(defn split-into-monotonics [a-seq]
  (reverse (mono-helper '() a-seq '())))

(defn perm-helper [prefix, a-set]
)

(defn permutations [a-set]
  (perm-helper '() a-set))

(defn powerset [a-set]
  [:-])
