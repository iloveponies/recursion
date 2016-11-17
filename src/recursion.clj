(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))
       )
  )

(defn singleton? [coll]
  (not (or (empty? coll) (not (empty? (rest coll))))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))
    ))

(defn max-element [a-seq]
  (if (< (count a-seq) 2)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
    ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
    ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (if (empty? (rest a-seq))
    (if (pred? (first a-seq)) (cons (first a-seq) ()) ())
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq)))
    ))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq)
        '()
        (pred? (first a-seq))
        (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else
        '()
        ))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq)
        '()
        (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
        :else
        a-seq
        ))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq)))
    false
    (and (empty? a-seq) (empty? b-seq))
    true
    (not (= (first a-seq) (first b-seq)))
    false
    :else
    (seq= (rest a-seq) (rest b-seq))
    ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
        ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
    ))

(defn fib [n]
  (cond (= n 0)
        0
        (= n 1)
        1
        :else
        (+ (fib (dec n)) (fib (dec (dec n))))
        ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))
    ))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] ())
    (cons a-seq (tails (rest a-seq)))
    ))


(defn inits [a-seq]
  (if (empty? a-seq)
    (cons [] ())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))
    ))


(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons [] ())
    (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))
    ))

(get {1 :a, 4 "tent"} (first [:a]))

(assoc {:a 3, 2 4} 2 :a)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
     (let [k (first a-seq)
           freq (get freqs k)]
       (if (= freq nil)
             (my-frequencies-helper (assoc freqs k 1) (rest a-seq))
             (my-frequencies-helper (assoc freqs k (inc freq)) (rest a-seq))
             ))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies [2 2 4 5 4])

(empty? [])

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[k f] (first a-map)
          a-seq (repeat f k)]
      (concat a-seq (un-frequencies (rest a-map)))
    )))

(let [[k f] (first {:a 2})
       a-seq (repeat f k)]
  (concat a-seq '()))

(concat [:a :a] [:b :b :b])

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1))
    ()
    (concat (vector (first coll)) (my-take (dec n) (rest coll)))
    ))

(concat (vector (first [1 2])) [3])

(defn my-drop [n coll]
  (if (or (empty? coll) (< n 1))
    coll
    (my-drop (dec n) (rest coll))
    ))


(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (vector (my-take h a-seq) (my-drop h a-seq))
    ))

(count ())

(defn seq-merge [a-seq b-seq]
  (cond (= 0 (count a-seq))
        b-seq
        (= 0 (count b-seq))
        a-seq
        (< (first a-seq) (first b-seq))
        (concat (vector (first a-seq)) (seq-merge (rest a-seq) b-seq))
        :else
        (concat (vector (first b-seq)) (seq-merge (rest b-seq) a-seq))
        ))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[fh sh] (halve a-seq)]
      (seq-merge (merge-sort fh) (merge-sort sh))
      )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

