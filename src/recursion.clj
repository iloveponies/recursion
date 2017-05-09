(ns recursion)

(defn product [coll]
  "Exercise 1"
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  "Exercise 3"
  (boolean (and (seq coll)
                (empty? (rest coll)))))


(defn my-last [coll]
  "Exercise 4"
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  "Exercise 5"
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  "Exercise 6"
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))


(defn longest-sequence [a-seq]
  "Exercise 7"
  (if (not (empty? a-seq))
    (if (empty? (rest a-seq))
      (seq-max (first a-seq) (rest a-seq))
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))


(defn my-filter [pred? a-seq]
  "Exercise 8"
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  "Exercise 9"
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  "Exercise 10"
  (let [first-elem (first a-seq)
        empty-coll '()]
    (cond
      (empty? a-seq) empty-coll
      (pred? first-elem) (cons first-elem (my-take-while pred? (rest a-seq)))
      :else
      empty-coll)))


(defn my-drop-while [pred? a-seq]
  "Exercise 11"
  (let [first-elem (first a-seq)
        rest-of-elems (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? first-elem)
        (my-drop-while pred? rest-of-elems)
        (cons first-elem rest-of-elems)))))


(defn seq= [a-seq b-seq]
  "Exercise 12"
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
    :else false))


(defn my-map [f seq-1 seq-2]
  "Exercise 13"
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [first-elem-1 (first seq-1)
          first-elem-2 (first seq-2)
          rest-1 (rest seq-1)
          rest-2 (rest seq-2)]
      (let [value (f first-elem-1 first-elem-2)]
        (cons value (my-map f rest-1 rest-2))))))


(defn power [n k]
  "Exercise 14"
  (if (== k 0)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  "Exercise 15"
  (if (or (== n 0) (== n 1))
    n
    (+ (fib (dec n)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  "Exercise 16"
  (let [empty-coll '()]
    (if (<= how-many-times 0)
      empty-coll
      (cons
        what-to-repeat
        (my-repeat (dec how-many-times) what-to-repeat)))))


(defn my-range [up-to]
  "Exercise 17"
  (let [empty-coll '()]
    (if (== up-to 0)
      empty-coll
      (cons
        (dec up-to)
        (my-range (dec up-to))))))


(defn tails [a-seq]
  "Exercise 16"
  [:-])

(defn inits [a-seq]
  "Exercise 17"
  [:-])

(defn rotations [a-seq]
  "Exercise 18"
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  "Exercise 19"
  [:-])

(defn my-frequencies [a-seq]
  "Exercise 20"
  [:-])

(defn un-frequencies [a-map]
  "Exercise 21"
  [:-])

(defn my-take [n coll]
  "Exercise 22"
  [:-])

(defn my-drop [n coll]
  "Exercise 23"
  [:-])

(defn halve [a-seq]
  "Exercise 24"
  [:-])

(defn seq-merge [a-seq b-seq]
  "Exercise 25 "
  [:-])

(defn merge-sort [a-seq]
  "Exercise 26"
  [:-])

(defn split-into-monotonics [a-seq]
  "Exercise 27"
  [:-])

(defn permutations [a-set]
  "Exercise 28"
  [:-])

(defn powerset [a-set]
  "Exercise 29"
  [:-])

