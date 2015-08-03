(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
   false
   (= elem (first a-seq))
   true
   :else
   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   a-seq
   (pred? (first a-seq))
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   a-seq
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
   true
   (or (empty? a-seq) (empty? b-seq))
   false
   (= (first a-seq) (first b-seq))
   (seq= (rest a-seq) (rest b-seq))
   :else
   false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2))
   '()
   :else
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
   0
   (== n 1)
   1
   :else
   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0)
   '()
   :else
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq)
   '(())
   :else
   (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rot-helper [n k a-seqq]
  (cond
   (empty? a-seqq)
   '(())
   (= n k)
   '()
   :else
   (cons a-seqq (rot-helper (inc n) k (concat (rest a-seqq) [(first a-seqq)])))))

(defn rotations [a-seq]
  (rot-helper 0 (count a-seq) a-seq))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)]
    (cond
     (empty? a-seq)
       freqs
     (not (contains? freqs f))
       (my-frequencies-helper (assoc freqs f 1) (rest a-seq))
     :else
       (my-frequencies-helper (update-in freqs [f] inc) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq freqs]
  (let [f (first freqs)]
  (cond
   (empty? freqs)
     a-seq
   :else
     (un-frequencies-helper (concat a-seq (repeat (second f) (first f))) (rest freqs)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take-helper [n coll a-seq]
  (if (and (> n 0) (not (empty? coll)))
    (my-take-helper (dec n) (rest coll) (concat a-seq [(first coll)]))
    a-seq))

(defn my-take [n coll]
  (my-take-helper n coll '()))

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

