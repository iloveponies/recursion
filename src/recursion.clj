(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (*
     (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (let [the-first (first coll)
        the-rest (rest coll)]
    (if (empty? the-rest)
      the-first
      (my-last the-rest))))


(defn max-element [a-seq]
  (let [[x & xs] a-seq]
    (if (empty? xs)
      x
      (max x (max-element xs)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (<= c1 c2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)
          pred-x (pred? x)
          xs (rest a-seq)
          filtered-xs (my-filter pred? xs)]
      (if pred-x
        (cons x filtered-xs)
        filtered-xs))))

(defn sequence-contains? [elem a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
     (empty? a-seq) false
     (= elem x)true
     :else (sequence-contains? elem xs))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [x (first a-seq)
          pred-x (pred? x)
          xs (rest a-seq)
          take-rest (my-take-while pred? xs)]
      (cond
       pred-x (cons x take-rest)
       :else '()))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (and
          (== (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+
          (fib (dec n))
          (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (seq ['()])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse
   (map
    reverse
    (tails
     (reverse a-seq)))))

(defn rotations [a-seq]
  (let [result (rest (map concat (tails a-seq) (inits a-seq)))]
    (if (empty? result)
      (seq ['()])
      result)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          current-val (get freqs x 0)
          new-freqs (assoc freqs x (inc current-val))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (seq [])
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [hc (int (/ (count a-seq) 2))]
    [(my-take hc a-seq) (my-drop hc a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge  a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (singleton? a-seq) a-seq
   :else (let [[h1 h2] (halve a-seq)]
           (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn seq-all-pair [pred s]
  (cond
   (empty? s) true
   (singleton? s) true
   :else (and (pred (first s) (second s)) (seq-all-pair pred (rest s)))))

(defn seq-is-monotonic? [s]
  (or (seq-all-pair <= s) (seq-all-pair >= s)))

(defn split-into-monotonics [a-seq]
  (let [a-seq-inits (inits a-seq)
        f-monot (my-last (take-while seq-is-monotonic? a-seq-inits))]
    (cond
      (empty? a-seq) '(())
      (= (count f-monot) (count a-seq)) (seq [(seq a-seq)])
      :else (cons f-monot (split-into-monotonics (drop (count f-monot) a-seq))))))

(defn permutations [a-set]
   (cond
    (empty? a-set) '(())
    (singleton? a-set) (list (seq a-set))
    :else
    (let [as-set (set a-set)
          permutations-of-subset-without-x (fn [x] (permutations (disj as-set x)))
          permutations-starting-with-x (fn [x] (map #(cons x %) (permutations-of-subset-without-x x)))]
        (apply concat (map #(permutations-starting-with-x %) as-set)))))

;(permutations [1 5 3])
;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))

;(set-union #{1} #{2})

;(clojure.set/union)

;(range 1)

;(set (range 2))

(defn add-to-all-sets [x seq-of-sets]
  (map #(conj % x) seq-of-sets))

(defn powerset [a-set]
  (let [as-set (set a-set)]
    (cond
     (empty? as-set) #{#{}}
     (singleton? as-set) #{as-set #{}}
     :else (let [powerset-of-subset-in-x (fn [x] (powerset (disj as-set x)))]
             (set (apply concat
                         (map (fn [x]
                                (let [sub-powerset (powerset-of-subset-in-x x)]
                                  (concat
                                   (add-to-all-sets x sub-powerset)
                                   sub-powerset)))
                              as-set)))))))

;(count (powerset (range 10)))
