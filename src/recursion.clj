(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))
      )
    ))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max
        (first a-seq)
        (max-element (rest a-seq)
    )))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if
    (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
  ))

(defn my-map [f seq-1 seq-2]
 (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))
   ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
    ))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    '()
    ))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (- up-to 1) (my-range (- up-to 1)))
    '()
    ))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))
    ))


(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))
    ))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [counted-freqs
          (if (contains? freqs (first a-seq))
            (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
            (assoc freqs (first a-seq) 1))
          ]
    (my-frequencies-helper counted-freqs (rest a-seq)))
    ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[k n]] (repeat n k)) a-map)
  ))

(defn my-take [n coll]
  (cond
    (zero? n) '()
    (empty? coll) coll
    :else (cons (first coll) (my-take (dec n) (rest coll)))
    ))


(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    ))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (apply seq-merge (map merge-sort (halve a-seq)))
    ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)
      ))

(defn split-into-monotonics [a-seq]
  (if (monotonic? a-seq)
    (list a-seq)
    (let [x (last (filter monotonic? (rest (inits a-seq))))]
      (cons x (split-into-monotonics (my-drop (count x) a-seq))))
    ))

(defn permutations [a-set]
  (let [helper (fn [[a & xs]] (map (fn [x] (cons a x)) (permutations xs)))]
    (if (empty? a-set)
      (list ())
      (apply concat (map helper (rotations a-set)))
    )))

(defn powerset [a-set]
  (let [helper (fn [x] (powerset (disj (set a-set) x)))]
    (if (empty? a-set)
      #{(set a-set)}
      (apply clojure.set/union #{(set a-set)} (map helper (set a-set)))
    )))

