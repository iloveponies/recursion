(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (first coll) (not (second coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (empty? (rest coll)) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq)) (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))



(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq)) (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (not (= elem (first a-seq)))
      (sequence-contains? elem (rest a-seq))
     :else
       true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (not (= (first a-seq) (first b-seq)))
    false
    :else (seq= (rest a-seq) (rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (zero? k)
        1
        (zero? n)
        0
        :else
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (== n 0)
        0
        (== n 1)
        1
        :else
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (== how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (concat [] [()])
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (let [new-freqs (assoc freqs (first a-seq) 1)]
            (my-frequencies-helper new-freqs (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[a b] (first a-map)]
      (concat (repeat b a) (un-frequencies(rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [osa1 (int (/ (count a-seq) 2))]
    [(my-take osa1 a-seq) (my-drop osa1 a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
      (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
      (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))
    )))

(defn merge-sort [a-seq]
  (if (or (== (count a-seq) 1) (== (count a-seq) 0))
    (seq-merge [] a-seq)
    (let [[eka toka] (halve a-seq)]
      (seq-merge (merge-sort eka) (merge-sort toka)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

