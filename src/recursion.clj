(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;(product [1 2 4])
;=> (* 1 (product [2 4]))
;=> (* 1 (* 2 (product [4])))
;=> (* 1 (* 2 (* 4 (product []))))
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (boolean (seq coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
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
;  (if (empty? a-seq)
;    a-seq
;    (if (pred? (first a-seq))
;      (cons (first a-seq)
;            (my-take-while pred? (rest a-seq)))
;      ())))
  (cond
    (empty? a-seq)
      a-seq

    (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))

    :else
      ()))

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
    (not= (count a-seq) (count b-seq))
      false

    (and (empty? a-seq) (empty? b-seq))
      true

    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))

    :else
      false))


(defn my-map [f seq-1 seq-2]
  (if (and (seq seq-1) (seq seq-2))
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))
    ()))

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
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (set (my-map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          new-count (if (contains? freqs element)
                      (inc (get freqs element))
                      1)
          new-freqs (assoc freqs element new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[element count] (first a-map)]
      (concat (repeat count element) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (and (>= n 1) (seq coll))
    (cons (first coll) (my-take (dec n) (rest coll)))
    ()))


(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [first-length (int (/ (count a-seq) 2))]
    [(my-take first-length a-seq) (my-drop first-length a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (empty? a-seq)
        b-seq
      (empty? b-seq)
        a-seq
      (< a b)
        (cons a (seq-merge (rest a-seq) b-seq))
      (> a b)
        (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)(apply >= a-seq)))

(defn first-monotonic [coll]
  (cond
    (empty? coll)
      [()]
    (monotonic? (first coll))
      (first coll)
    :else
      (first-monotonic (rest coll))))

(defn split-into-monotonics [a-seq]
  (let [monotonic-piece (first-monotonic (inits a-seq))]
    (if (monotonic? a-seq)
      [a-seq]
      (cons monotonic-piece
            (split-into-monotonics (drop (count monotonic-piece) a-seq)))
      )))

(defn permutations [a-set]
  (let [keep-firsts-permutate-rest (fn [x] (map cons (repeat (first x))
                                                     (permutations (rest x))))]
    (if (empty? a-set)
      [()]
      (apply concat (map keep-firsts-permutate-rest (rotations a-set))))))

(defn powerset [a-set]
  (cond
    (empty? a-set)
      #{#{}}

    (= (count a-set) 1)
      (conj #{#{}} (set a-set))
))
;    :else
;      (conj (set (map powerset (map rest (permutations a-set)))) (set a-set))))
