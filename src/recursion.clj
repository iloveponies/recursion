(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not (empty? coll)))
    true
    false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max-element (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (and (or (singleton? a-seq) (empty? a-seq)))
     (first a-seq)
   (= (seq-max (first a-seq) (first (rest a-seq))) (first a-seq))
     (longest-sequence (conj (rest (rest a-seq)) (first a-seq)))
   :else
     (longest-sequence (rest a-seq))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else
     (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (not (pred? (first a-seq)))
     (seq a-seq)
   :else
     (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== 0 n k)
    (str "Undefined")
   (== 0 k)
     1
   (== 0 n)
     0
   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (< n 0)
     (str "Undefined")
   (= n 0)
     0
   (= n 1)
     1
   :else
     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails-helper [a-seq]
  (if (empty? a-seq)
    ()
    (cons (seq a-seq) (tails-helper (rest a-seq)))))

(defn tails [a-seq]
  (seq (set (cons () (tails-helper a-seq)))))

(defn inits-helper [a-seq]
  (if (empty? a-seq)
    ()
    (cons (seq a-seq) (inits-helper (reverse (rest (reverse a-seq)))))))

(defn inits [a-seq]
  (seq (set (cons () (inits-helper a-seq)))))

(defn rotations-helper [n a-seq]
  (cond
   (empty? a-seq)
     (conj () ())
   (== n (count a-seq))
     ()
   :else
     (cons (seq a-seq)
           (rotations-helper (inc n) (concat (rest a-seq) [(first a-seq)])))))

(defn rotations [a-seq]
  (rotations-helper 0 a-seq))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll)
     ()
   (<= n 0)
     (seq coll)
   :else
     (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq)
   (my-drop (int (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     ()
   (empty? a-seq)
     (cons (first b-seq) (seq-merge () (rest b-seq)))
   (empty? b-seq)
     (cons (first a-seq) (seq-merge (rest a-seq) ()))
   (<= (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (> (first a-seq) (first b-seq))
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [n (count a-seq)]
    (if (<= n 1)
      a-seq
      (let [[first second] (halve a-seq)]
        (seq-merge (merge-sort first) (merge-sort second))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

