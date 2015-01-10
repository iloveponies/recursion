(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (== (count (take 2 coll)) 1))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   :else
   (if (singleton? coll) (first coll)
     (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq)) (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) (first a-seq) (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
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
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) (rest a-seq)
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not(== (count a-seq) (count b-seq))) false
   (and (singleton? a-seq) (singleton? b-seq)) (= (first a-seq) (first b-seq))
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) (rest seq-2)
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (let [fibs
        (take (+ n 1)
              (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))] (my-last fibs)))

(defn repeat-help [how-many-times what-to-repeat seq]
  (if (zero? how-many-times)
    seq
    (cons what-to-repeat (repeat-help (dec how-many-times) what-to-repeat seq))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 0)
    ()
    (repeat-help how-many-times what-to-repeat ())))

(defn my-range-help [up-to seq]
  (if (neg? up-to)
    seq
    (cons up-to (my-range-help (dec up-to) seq))))

(defn my-range [up-to]
  (my-range-help (dec up-to) ()))

(defn tails-help [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (cons (rest a-seq) (tails-help (rest a-seq) b-seq))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
    (let [b (cons 0 a-seq)]
      (tails-help b ()))))

(defn inits-help [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (cons (rest a-seq) (tails-help (rest a-seq) b-seq))))

(defn inits [a-seq]
  (if (empty? a-seq) [[]]
    (let [b (reverse a-seq)]
      (map reverse (inits-help (cons (+ 1 (max-element b)) b) ())))))

(defn rotations [a-seq]
  (if (empty? a-seq) (cons () a-seq)
    (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))  (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map]
  )

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map))

(defn my-take [n coll]
  (take n coll))

(defn my-drop [n coll]
  (drop n coll))

(defn halve [a-seq]
  (let [a (int (/ (count a-seq) 2))]
    (vector (my-take a a-seq) (my-drop a a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
   '()
   (empty? a-seq)
   (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq)
   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
   (if (> (first a-seq) (first b-seq))
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (seq-merge (get (halve a-seq) 1) (get (halve a-seq) 0))
    (seq-merge (merge-sort (get (halve a-seq) 1)) (merge-sort (get (halve a-seq) 0)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  )

(defn powerset [a-set]
  )

