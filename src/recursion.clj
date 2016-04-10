(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (next coll))))


(defn my-last [coll]
  (cond
    (empty? coll)
      nil
    (singleton? coll)
      (first coll)
    :else
      (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
   (if(> len-1 len-2)
     seq-1
     seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-2) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) (cons () ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) (cons () ())
    (cons a-seq (inits (pop a-seq)))))

(defn my-rotations [a-seq n]
  (if (zero? n) ()
    (cons (seq a-seq) (my-rotations (cons (last a-seq) (drop-last a-seq)) (dec n)))))

(defn rotations [a-seq]
  (if (zero? (count a-seq)) [()]
    (my-rotations a-seq (count a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq)) (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map) a-seq
    (let [current (first a-map)
          [k v] current]
    (un-frequencies-helper (concat a-seq (repeat v k)) (rest a-map)))))

(defn un-frequencies [a-map]
  (if (empty? a-map) [()]
    (un-frequencies-helper () a-map)))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll)) ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll)) coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [divisor (int (/ (count a-seq) 2))]
    (vector (my-take divisor a-seq) (my-drop divisor a-seq))))

(defn seq-merge-helper [a-seq b-seq sorted]
  (cond
    (empty? a-seq)
    (concat sorted b-seq)
    (empty? b-seq)
    (concat sorted a-seq)
    :else
    (let [first-a (first a-seq) first-b (first b-seq)]
      (if (< first-a first-b)
        (seq-merge-helper (rest a-seq) b-seq (concat sorted [first-a]))
        (seq-merge-helper a-seq (rest b-seq) (concat sorted [first-b]))))))

(defn seq-merge [a-seq b-seq]
    (seq-merge-helper a-seq b-seq '()))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [ [a b] (halve a-seq) ]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
