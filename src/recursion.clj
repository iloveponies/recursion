(ns recursion
  (:use clojure.set))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max
      (first a-seq)
      (max-element (rest a-seq)))))

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
    (let [first-element (first a-seq)
          filtered-rest (my-filter pred? (rest a-seq))]
      (if (pred? first-element)
        (cons first-element filtered-rest)
        filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or
      (= elem (first a-seq))
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      `())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    `()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    `()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    `()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    `(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [element (first a-seq)
          new-count (inc (or (freqs element) 0))
          new-freqs (assoc freqs element new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    `()
    (let [[value repetitions] (first a-map)
          seq-of-values (repeat repetitions value)]
      (concat seq-of-values (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    `()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq), (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [first-a (first a-seq)
                first-b (first b-seq)
                rest-a (rest a-seq)
                rest-b (rest b-seq)
                a-goes-now (<= first-a first-b)]
            (if a-goes-now
              (concat [first-a] (seq-merge rest-a b-seq))
              (concat [first-b] (seq-merge a-seq rest-b))))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))
;
;(defn inits-shortest-first [a-seq] (reverse (inits (a-seq))))


(defn get-longest-monotonic [a-seq]
  (let [inits (inits a-seq)
        is-monotonic? (fn [a-seq]
                        (or
                          (apply <= a-seq)
                          (apply >= a-seq)))
        ]
    (first (drop-while (complement is-monotonic?) inits)))
  )


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [longest-monotonic (get-longest-monotonic a-seq)
          longest-monotonic-count (count longest-monotonic)
          rest-of-sequence (drop longest-monotonic-count a-seq)]
      (cons longest-monotonic (split-into-monotonics rest-of-sequence))))
  )

(defn permutations [a-set]
  (if (empty? a-set)
    `(())
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (cons head tail))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [first-elem (first a-set)
          powerset-of-rest-without-first (powerset (rest a-set))
          powerset-of-rest-with-first (map #(conj %1 first-elem) powerset-of-rest-without-first)]
      (union powerset-of-rest-without-first powerset-of-rest-with-first))))

