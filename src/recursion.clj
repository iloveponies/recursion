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
  (cond
    (singleton? coll) (first coll)
    (empty? coll) nil
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq) nil
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [l1 (count seq-1) l2 (count seq-2)]
  (if (> l1 l2)
    seq-1
    seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq) nil
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not (= (first a-seq) elem))
      (sequence-contains? elem (rest a-seq))
    :else
      true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) []
    :else
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq))
       (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (not (or (empty? seq-1) (empty? seq-2)))
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    :else []))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [seq (if (empty? a-seq) [] (tails (rest a-seq)))]
    (cons a-seq seq)))

(defn inits [a-seq]
  (if (empty? a-seq) [()]
  (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq) [()]
  (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq) cont (if (contains? freqs f) (inc (get freqs f)) 1)]
  (cond
    (empty? a-seq) freqs
    :else (my-frequencies-helper (assoc freqs f cont) (rest a-seq)))))

(defn my-frequencies[a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) ()
    :else
    (concat
      (repeat (val (first a-map)) (key (first a-map)))
      (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (< n 1) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (< n 1) coll
    :else (my-drop (dec n) (rest coll))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (< n 1) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (< n 1) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [cut (int (/ (count a-seq) 2))]
  (vector (my-take cut a-seq) (my-drop cut a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1) a-seq
    :else (seq-merge (merge-sort (get (halve a-seq) 0)) (merge-sort (get (halve a-seq) 1)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

