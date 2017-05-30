(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

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
      (if (singleton? a-seq)
        a-seq
      (cons (first a-seq) (my-filter pred? (rest a-seq))))
       (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
    (if (singleton? a-seq)
        a-seq
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))
    ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (== 0 n)
    0
    (if (== 1 n)
      1
      (+ (fib (dec n)) (fib (- n 2))))))

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
    (cons () ())
    (cons (reverse (reverse a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons (reverse (reverse a-seq)) (inits (reverse(rest (reverse a-seq)))))))

(defn rotations-helper [a-seq k]
  (if (== k 0)
    ()
    (cons (reverse (reverse a-seq))
          (rotations-helper (reverse(conj (reverse (rest a-seq)) (first a-seq))) (dec k)))))


(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (rotations-helper a-seq (count a-seq))))



(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (= (get freqs (first a-seq)) nil)
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq)
        (inc (get freqs (first a-seq)))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (get a-map (first (first a-map))) (first (first a-map)))
            (un-frequencies (dissoc a-map (first (first a-map)))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq) ])

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (if (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (reverse (reverse a-seq))
    (seq-merge (merge-sort (my-take (int (/ (count a-seq) 2)) a-seq)) (merge-sort (my-drop (int (/ (count a-seq) 2)) a-seq)))))

(defn monotonic [a-seq mode]
  (if (singleton? a-seq)
    (reverse a-seq)
    (if (== mode 1)
    (if (< (first a-seq) (first (rest a-seq)))
      (cons (first a-seq) (monotonic (rest a-seq) 1))
      (reverse [(first a-seq)]))
      (if (== mode -1)
        (if (> (first a-seq) (first (rest a-seq)))
      (cons (first a-seq) (monotonic (rest a-seq) -1))
      (reverse [(first a-seq)]))
        (if (< (first a-seq) (first (rest a-seq)))
      (cons (first a-seq) (monotonic (rest a-seq) 1))
          (cons (first a-seq) (monotonic (rest a-seq) -1)))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [lt (count (monotonic a-seq 0))] (cons (monotonic a-seq 0) (split-into-monotonics (my-drop lt a-seq))))))




(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

