(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (get coll (- (count coll) 1)))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
  (if (singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) ()
    (if (pred? (first a-seq))
           (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== elem (first a-seq))
     true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     ()
   ))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
  (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq)) a-seq)))

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
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
     (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
   (let [seqi (lazy-seq a-seq)]
     (if (<= (count seqi) 0)
          (conj () ())
          (cons seqi (tails (rest seqi))))))

(defn inits-helper [a-seq n]
  (let [b-seq (lazy-seq a-seq)]
   (if (or (empty? b-seq) (= n 0))
     (conj () ())
     (if (= n (count a-seq))
       (reverse (cons b-seq (inits-helper (drop-last b-seq) n)))
       (cons b-seq (inits-helper (drop-last b-seq) n))))))

(defn inits [a-seq]
  (inits-helper a-seq (count a-seq)))

(defn rotations-helper [a-seq n]
  (let [b-seq (lazy-seq a-seq)
        last-one (last a-seq)
        others (drop-last a-seq)]
  (if (= n 0)
    ()
    (cons b-seq (rotations-helper (conj others last-one) (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (conj () ())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (= (get freqs (first a-seq)) nil)
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (> n (count coll))
    ()
  (if (or (= n 0) (empty? coll))
    (cons (first coll) (rest coll))
    (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (into [] (conj (conj () (my-drop n a-seq)) (my-take n a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (>= (first a-seq) (first b-seq))
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   :else
     (cons (first a-seq) (seq-merge b-seq (rest a-seq)))))

(defn merge-sort [a-seq]
  (let [a-list (halve a-seq)]
  (if (or (= (count a-seq) 0) (= (count a-seq) 1))
    a-seq
    (seq-merge (merge-sort (first a-list)) (merge-sort (second a-list))))))

(defn mon-helper [a-seq is-pos]
  (let [one (first a-seq)
        other (second a-seq)
        others (rest a-seq)]
  (if (singleton? a-seq)
    true
    (if is-pos
      (if (>= other one) (mon-helper others true) false)
      (if (<= other one) (mon-helper others false) false)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (mon-helper a-seq true) (mon-helper a-seq false))))

(defn sim-helper [a-seq all-list]
  (let [inits-list (inits a-seq)
        seqi (last (take-while monotonic? inits-list))
        rest-list (drop (count seqi) a-seq)]
    (if (empty? a-seq)
      all-list
      (sim-helper rest-list (cons seqi all-list)))))

(defn split-into-monotonics [a-seq]
  (reverse (sim-helper a-seq [])))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

