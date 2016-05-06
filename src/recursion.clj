(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll) (empty? (rest coll))))

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
      (max (max-element (rest a-seq)) (first a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (longest-sequence (rest a-seq))
                      (first a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0)
      0
    (== n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

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
    (list a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ( cons () () )
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [counter (if (contains? freqs (first a-seq))
                    (get freqs (first a-seq))
                    0)]
      (my-frequencies-helper (assoc freqs (first a-seq) (inc counter)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
 (reverse (my-take (- (count coll) n ) (reverse coll))))

(defn halve [a-seq]
  (if (odd? (count a-seq))
    [(my-take (int (/ (count a-seq) 2)) a-seq)
     (my-drop (dec (- (count a-seq) (int (/ (count a-seq) 2)))) a-seq)]
    [(my-take (int (/ (count a-seq) 2)) a-seq)
     (my-drop (- (count a-seq) (int (/ (count a-seq) 2))) a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (<= (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (== (count a-seq) 0) a-seq
    (== (count a-seq) 1) a-seq
    :else (let [[s1 s2] (halve a-seq)]
            (seq-merge (merge-sort s1) (merge-sort s2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

