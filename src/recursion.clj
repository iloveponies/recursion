(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (if (empty? (rest coll))
      (first coll)
      (* (first coll)
         (product (rest coll))))))

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
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (my-last a-seq)
      (seq-max (longest-sequence (rest a-seq)) (my-last a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (if (and (empty? a-seq) (empty? b-seq))
      true
      false)
    (if (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
     0
   (== 1 n)
     1
   :else
     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn cut-tail [a-seq]
  (reverse (rest (reverse a-seq))))

(defn do-inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (do-inits (cut-tail a-seq)))))

(defn inits [a-seq]
  (reverse (do-inits a-seq)))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  (let [new-freqs
        (if (sequence-contains? (first a-seq) (keys freqs))
          (update-in freqs [(first a-seq)] inc)
          (assoc freqs (first a-seq) 1))]
    (if (singleton? a-seq)
      new-freqs
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies-helper [a-seq freqs]
  (if (empty? freqs)
    a-seq
    (un-frequencies-helper (concat a-seq (repeat (second (first freqs)) (first (first freqs)))) (rest freqs))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if (zero? n)
      (cons (first coll) (my-drop n (rest coll)))
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (vec (concat [(my-take (int (/ (count a-seq) 2)) a-seq)] [(my-drop (int (/ (count a-seq) 2)) a-seq)])))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    (if (empty? b-seq)
      ()
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))
    (if (empty? b-seq)
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (first (halve a-seq)) (second (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

