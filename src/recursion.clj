(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '() ))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[x n]] (repeat n x)) a-map)))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (<= n 0) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-point (int (/ (count a-seq) 2))]
    [(my-take split-point a-seq)
     (my-drop split-point a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq)) (concat a-seq b-seq)
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
  (let [[a b] (halve a-seq)]
    (if (<= (count a-seq) 1) a-seq
    (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic? [a-seq]
  (if (>= 1 (count a-seq)) true
   (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (let [monotonic-inits (filter monotonic? (inits a-seq))
        n (dec (count monotonic-inits))]
    (if (empty? a-seq) '()
    (cons (take n a-seq) (split-into-monotonics (drop n a-seq))))))

(defn set->permutations [a-set]
  (cond
   (empty? a-set) '(())
   (singleton? a-set) (list (list (first a-set)))
   :else (apply concat (map (fn [x] (map (fn [res] (cons x res) )
                                         (set->permutations (disj a-set x))))
                            a-set))))

(defn permutations [a-set]
  (set->permutations (set a-set)))

(defn set->powerset [a-set]
  (cond
   (empty? a-set) #{a-set}
   :else (apply clojure.set/union (map (fn [x] (let [child (set->powerset (disj a-set x))]
                                    (clojure.set/union
                                    child ;set without self
                                    (map (fn [c-set] (conj c-set x)) child ))));set with self
                           a-set))))

(defn powerset [a-set]
  (set (vec(set->powerset (set a-set)))))
