(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not (empty? coll) ))
    true
    false
    ))

(defn my-last [coll]
  (if (or (singleton? coll) (nil? (first coll)))
    (first coll)
    (my-last (rest coll))
    ))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if(> (count seq-1) (count seq-2))
    seq-1
    seq-2
   ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons(first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
 (cond
  (empty? a-seq) a-seq
  (pred? (first a-seq))  ( my-drop-while pred? (rest a-seq))
  :else  a-seq))
(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
  1
  (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
    :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
   (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a ( first a-seq)
          cnt (inc (get freqs a 0))]
      (my-frequencies-helper (assoc freqs a cnt) (rest a-seq)))))

(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map keyys]
  (if (empty? keyys)
    {}
    (concat (repeat (a-map (first keyys)) (first keyys)) (un-frequencies-helper a-map (rest keyys) ))))

(defn un-frequencies [a-map]
    (un-frequencies-helper a-map (keys a-map)))

(defn my-take [n coll]
   (cond
    (not (or (empty? coll) (zero? n))) (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve-helper [a-seq start end]
  (cond
   (zero? start) (my-take end a-seq)
   :else (my-drop start a-seq)))

(defn halve [a-seq]
  (let [seq-count (count a-seq)
        seq-size (int (/ seq-count 2 ))
        seq-mod (mod seq-count 2)
       ]
    (cond
     (zero? seq-count) [(list) (list)]
     (= 1 seq-count) (vector (list) a-seq)
     :else (vector (halve-helper a-seq 0 seq-size ) (halve-helper a-seq seq-size 0)))))


(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

