(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (-> coll empty? not) (-> coll rest empty?)))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element
  ([a-seq] (max-element max a-seq))
  ([max-fn a-seq]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) (first a-seq)
      :else (max-fn (first a-seq) (max-element max-fn (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (max-element seq-max a-seq))

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
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (empty? b-seq) (not (empty? a-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (->> a-seq reverse tails reverse (map reverse)))

(defn rotate [n a-seq]
  (let [ct (count a-seq)]
    (take ct (drop (mod n ct) (cycle a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (map #(rotate % a-seq) (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (update-in freqs [(first a-seq)] #(inc (or % 0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (for [[elem ct] (vec a-map) n (range ct)] elem))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (== n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-size (int (/ (count a-seq) 2))]
    [(my-take half-size a-seq) (my-drop half-size a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    (sequence a-seq)
    (let [[b-seq c-seq] (halve a-seq)]
      (seq-merge (merge-sort b-seq) (merge-sort c-seq)))))

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(defn monotonic-prefix [a-seq]
  (if (empty? a-seq)
    '()
    (let [consec-items (consecutives a-seq)
          mono-inc (take-while (fn [[a b]] (<= a b)) consec-items)
          mono-dec (take-while (fn [[a b]] (>= a b)) consec-items)]
      (if (empty? mono-inc)
        (cons (first a-seq) (map last mono-dec))
        (cons (first a-seq) (map last mono-inc))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [p (monotonic-prefix a-seq)]
      (cons p (split-into-monotonics (drop (count p) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [a-set (if (set? a-set) a-set (set a-set))
          permutations-for-head (fn [head-elem] (map #(cons head-elem %) (permutations (disj a-set head-elem))))]
      (apply concat (for [elem a-set] (permutations-for-head elem))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{a-set}
    (let [a-set (if (set? a-set) a-set (set a-set))
          elem (first a-set)
          sub-powerset (powerset (disj a-set elem))
          with-elem (map #(conj % elem) sub-powerset)]
      (clojure.set/union sub-powerset with-elem))))

(comment
  (consecutives [:a :b :c]) ;=> ([:a :b] [:b :c])
  (consecutives [1 2 3 4])  ;=> ([1 2] [2 3] [3 4])
)
