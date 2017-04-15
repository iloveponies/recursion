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
      (max (max-element (rest a-seq))))))

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
  (if (empty? a-seq)
    false
    (if (== (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq))
      )
  ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    :else (if (pred? (first a-seq))
            (cons (first a-seq) (my-take-while pred? (rest a-seq)))
            ())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq))
            (my-drop-while pred? (rest a-seq))
            a-seq
            )))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   ))

(defn power [n k]
  (cond
    (== k 1) n
    (== k 0) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()
    ))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    ()))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))
    ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))



(defn rotations [a-seq]
  (let [tail-of-length (fn [len] (filter (fn [seq] (== len (count seq))) (tails a-seq)))
        init-of-length (fn [len] (filter (fn [seq] (== len (count seq))) (inits a-seq)))
        rotation (fn [x]
                   (apply concat
                    (apply concat (tail-of-length x))
                    (init-of-length (- (count a-seq) x))))
        ]
  (if (empty? a-seq)
    [()]
    (map rotation (my-range (count a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc
                             freqs
                             (first a-seq)
                             (if (contains? freqs (first a-seq))
                               (inc (get freqs (first a-seq)))
                               1
                               )
                             )
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [x] (repeat (val x) (key x))) a-map)))

(defn my-take [n coll]
  (if (and (not (empty? coll)) (pos? n))
    (cons (first coll) (my-take (dec n) (rest coll)))
    []))

(defn my-drop [n coll]
  (cond
   (empty? coll) []
   (< 0 n) (my-drop (dec n) (rest coll))
   :else coll
   ))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2.0))
            a-seq)
   (my-drop (int (- (count a-seq) (/ (count a-seq) 2.0)))
            a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()

    (empty? a-seq)
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))

    (empty? b-seq)
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))

    (<= (first a-seq) (first b-seq))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))

    (>= (first a-seq) (first b-seq))
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[first second] (halve a-seq)]
    (seq-merge (merge-sort first) (merge-sort second)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

