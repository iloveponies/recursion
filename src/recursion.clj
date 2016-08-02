(ns recursion)

(defn product [coll]
  (if (not (empty? coll))
    (* (first coll)
       (product (rest coll)))
    1)
  )

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll)))
  )

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll))))
  )

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq)))))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2)
  )

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (= false (pred? (first a-seq)))
      (my-filter pred? (rest a-seq))
      (cons (first a-seq)
          (my-filter pred? (rest a-seq)))))
  )

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
   :else true)
  )

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '())
  )

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq)
  )

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false)
  )

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1))))
  )


(defn fib [n]
  (cond
    (= 1 n) 1
    (= 0 n) 0
    :else (+
          (fib (dec n))
          (fib (dec (dec n)))))
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))
  )

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to))))
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    (seq ['()])
    (cons (seq a-seq) (tails (rest a-seq))))
  )

(defn inits [a-seq]
  (reverse
    (map
      reverse
        (tails
          (reverse a-seq))))
  )

(defn rotations [a-seq]
  (let [result (rest (map concat (tails a-seq) (inits a-seq)))]
    (if (empty? result)
      (seq ['()])
        result))
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          current-val (get freqs x 0)
          newfq (assoc freqs x (inc current-val))]
      (my-frequencies-helper newfq (rest a-seq))))
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  (apply concat (map
                  (fn [x] (repeat (get a-map x) x))
                  (keys a-map)))
  )

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
      '()
      (cons (first coll) (my-take (- n 1) (rest coll))))
  )

(defn my-drop [n coll]
  (if (<= (count coll) n) []
  (reverse (my-take (- (count coll) n) (reverse coll))))
  )

(defn halve [a-seq]
  (cons
    (my-take (int (/ (count a-seq) 2)) a-seq)
    (cons (my-drop (int (/ (count a-seq) 2)) a-seq) nil))
  )

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (concat [(first a-seq)] (seq-merge b-seq (rest a-seq)))
   :else (concat [(first b-seq)] (seq-merge a-seq (rest b-seq))))
  )

(defn merge-sort [a-seq]
  (cond
   (or
     (empty? a-seq)
     (= (count a-seq) 1)) a-seq
   :else (seq-merge
           (merge-sort (vec (first (halve a-seq))))
           (merge-sort (vec (second (halve a-seq))))))
  )

(defn monotonic? [a-seq]
  (or
    (= (sort a-seq) a-seq)
    (= (reverse (sort a-seq)) a-seq))
  )

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic (->> a-seq inits (take-while monotonic?) last)]
    (cons monotonic (split-into-monotonics (drop (count monotonic) a-seq)))))
  )

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat
      (fn [x] (map (fn [y] (cons x y)) (permutations (remove (partial = x) a-set)))) a-set))
  )

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
      (let [rest-of-the-sets (powerset (rest a-set))
            combined-sets (map (fn [x] (conj x (first a-set))) rest-of-the-sets)]
        (clojure.set/union combined-sets rest-of-the-sets)))
  )
