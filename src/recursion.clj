(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
     (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
     nil
      (if (singleton? a-seq)
        (first a-seq)
          (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
     '()
     (if (pred? (first a-seq))(cons (first a-seq)(my-filter pred? (rest a-seq)))
       (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false (if (= elem (first a-seq)) true (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
  (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
     a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (empty? b-seq) (not (empty? a-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
    (== 0 k) 1
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (== up-to 0) '()
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
 (if (empty? a-seq) ['()] (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (cond (== (count a-seq) 1) a-seq
        (empty? a-seq) ['()]
  :else (rest (reverse (map concat (tails a-seq) (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [first-elem (first a-seq)
          is-elem? (fn [x] (= x first-elem))]
    (cond
     (empty? a-seq) freqs
     :else (my-frequencies-helper (assoc freqs first-elem (count (filter is-elem? a-seq))) (filter (complement is-elem?) a-seq)))))



(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (let [[a b] (first a-map)]
    (concat (repeat b a) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll)) '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll)) coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
     [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (== (first a-seq) (first b-seq)) (cons (first a-seq) (cons (first b-seq) (seq-merge (rest a-seq) (rest b-seq))))
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) a-seq (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (first (rest (halve a-seq)))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
  (let [monotonic? (fn [b-seq] (if (empty? b-seq) true (or (apply <= b-seq) (apply >= b-seq))))
        current-piece (first (reverse (take-while monotonic? (inits a-seq))))]
    (cons current-piece (split-into-monotonics (drop (count current-piece) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

