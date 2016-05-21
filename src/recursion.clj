(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? (seq coll)) false
    (empty? (seq(rest coll))) true
    :else false))

(defn my-last [coll]
  (cond
    (empty? (seq coll)) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (== (first a-seq) elem)
    true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not(== (count a-seq) (count b-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    (and (empty? a-seq) (empty? a-seq)) true
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map sort (tails (reverse a-seq)))))

(defn rotate-helper [sq rots]
  (if (= 0 rots)
    ()
    (cons (seq sq) (rotate-helper (cons (last sq) (drop-last sq)) (dec rots)))))

(defn rotations [a-seq]
  (if (empty? a-seq) [()]
    (rotate-helper a-seq (count a-seq))))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq)) (my-frequencies-helper freqs (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) (count-elem (first a-seq) a-seq)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) ()
    (concat (repeat (last (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll)) ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop-helper [m n coll]
  (if (== m n) coll
    (my-drop-helper (inc m) n (rest coll))))

(defn my-drop [n coll]
  (if (or (< (count coll) n)
          (empty? coll))
    ()
    (my-drop-helper 0 n coll)))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (last (halve a-seq))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

