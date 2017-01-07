(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq)
       (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq)
       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if
      (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
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
    (not (= (count a-seq) (count b-seq)))
     false
    (and (empty? a-seq) (empty? b-seq))
     true
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? n)
    0
    (if (zero? k)
      1
      (* n (power n (dec k))))))

(defn fib [n]
  (if (< n 2)
    n
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
  (if (empty? a-seq) (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq) (list ())
  (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs
          (assoc freqs (first a-seq) (if (contains? freqs (first a-seq)) (inc (get freqs (first a-seq))) 1))]
      (my-frequencies-helper new-freqs
                         (rest a-seq)))))

(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0))
        (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond (zero? n)
      []
    (> n (count coll))
      (my-take (count coll) coll)
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (or (empty? coll) (> n (count coll)))
      ()
    (> n 0)
      (my-drop (dec n) (rest coll))
    :else
      (cons (first coll) (my-drop 0 (rest coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (cons (my-take half a-seq) (list (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq)
          b-seq
        (empty? b-seq)
          a-seq
    :else
        (let [smaller (if (< (first a-seq) (first b-seq)) a-seq b-seq)
          other (if (= smaller a-seq) b-seq a-seq)]
        (cons (first smaller) (seq-merge other (rest smaller))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
  (let [halfs (halve a-seq)
        sortFirst (merge-sort (first halfs))
        sortSecond (merge-sort (second halfs))]
      (seq-merge sortFirst sortSecond))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

