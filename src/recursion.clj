(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max
      (first a-seq)
      (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons
      (first a-seq)
      (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons
      what-to-repeat
      (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons
      (dec up-to)
      (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (= 0 (count a-seq))
    (cons () a-seq)
    (cons
      (seq a-seq)
      (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (distinct
      (my-map
        (fn [a b] (concat a b))
        (tails a-seq)
        (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)]
      (let [freq (get freqs fst 0)]
        (my-frequencies-helper
          (assoc freqs fst (inc freq))
          (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq freqs]
  (if (empty? freqs)
    a-seq
    (let [fst (first freqs)]
      (un-frequencies-helper
        (concat a-seq (repeat (val fst) (key fst)))
        (rest freqs)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take-helper [acc items n coll]
  (if (or (= acc n) (empty? coll))
    items
    (my-take-helper (inc acc) (conj items (first coll)) n (rest coll))
  )
)

(defn my-take [n coll]
  (my-take-helper 0 [] n coll))

(defn my-drop-helper [acc n coll]
  (if (or (= acc n) (empty? coll))
    coll
    (my-drop-helper (inc acc) n (rest coll))))

(defn my-drop [n coll]
  (my-drop-helper 0 n coll))

(defn halve-helper [acc n first-seq last-seq]
  (if (or (= acc n) (empty? last-seq))
    [first-seq last-seq]
    (halve-helper
      (inc acc)
      n
      (concat first-seq [(first last-seq)])
      (rest last-seq))))

(defn halve [a-seq]
  (halve-helper 0 (int (/ (count a-seq) 2)) [] a-seq))

(defn seq-merge-helper [acc a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) acc
    (empty? a-seq) (concat acc b-seq)
    (empty? b-seq) (concat acc a-seq)
    :else (let [a-first (first a-seq)
                b-first (first b-seq)]
      (if (< a-first b-first)
        (seq-merge-helper (concat acc [a-first]) (rest a-seq) b-seq)
        (seq-merge-helper (concat acc [b-first]) a-seq (rest b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [split (halve a-seq)]
      (seq-merge
        (merge-sort (get split 0))
        (merge-sort (get split 1))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
