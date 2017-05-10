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
  "Exercise 18"
  (let [empty-coll ['()]]
    (if (empty? a-seq)
      empty-coll
      (cons
        (seq a-seq)
        (tails (rest a-seq))))))


(defn inits [a-seq]
  "Exercise 18"
  (map reverse (tails (reverse a-seq))))


(defn rotations [a-seq]
  "Exercise 19"
  (let [init (inits a-seq)
        tail (reverse (tails a-seq))
        init-tail (map vector tail init)]
    (distinct (map (fn [x] (apply concat x)) init-tail))))


(defn frequencies-helper [freqs a-seq]
  "Exercise 20"
  (if (empty? a-seq)
    freqs
    (let [first-elem (first a-seq)
          rest-elems (rest a-seq)
          the-count (inc (if (contains? freqs first-elem)
                           (get freqs first-elem)           ;Returns the value mapped to the key
                           0))]
      (frequencies-helper (assoc freqs first-elem the-count) rest-elems)))) ; assoc new count to the element

(defn my-frequencies [a-seq]
  "Exercise 20"
  (frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  "Exercise 21"
  (let [first-elem (first a-map)
        elem-val (first first-elem)
        elem-count (second first-elem)
        repeated-elems (repeat elem-count elem-val)]
    (if (== 1 (count a-map))
      repeated-elems
      (concat repeated-elems (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  "Exercise 22"
  (let [nothing-to-return (or (empty? coll) (< n 1))
        empty-coll '()]
    (if nothing-to-return
      empty-coll
      (concat [(first coll)] (my-take (dec n) (rest coll))))))


(defn my-drop [n coll]
  "Exercise 23"
  (let [no-dropped-elems (< n 1)
        empty-coll (empty? coll)]
    (cond
      empty-coll coll
      no-dropped-elems coll
      :else (my-drop (dec n) (rest coll)))))


(defn halve [a-seq]
  "Exercise 24"
  (let [middle-point (int (/ (count a-seq) 2))]
    (concat [(my-take middle-point a-seq)] [(my-drop middle-point a-seq)])))


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

