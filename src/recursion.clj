(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (= nil (second coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [coll]
  (if (singleton? coll)
    (first coll)
    (seq-max (first coll) (longest-sequence (rest coll)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
               (cons
                 (first a-seq)
                 (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (= elem (first a-seq))
    true
    (if (or (empty? a-seq) (singleton? a-seq))
      false
      (recur elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) `()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else `()))

(defn alwaysTrue [x] true)

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) `()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (my-take-while alwaysTrue a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
    :else                               false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    `()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))



(defn my-repeat-r [daList how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times) daList
    :else (conj (my-repeat-r daList (- how-many-times 1) what-to-repeat) what-to-repeat)))

(defn my-repeat [how-many-times what-to-repeat]
  (my-repeat-r `() how-many-times what-to-repeat))

(my-repeat 3 "lol")

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

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

