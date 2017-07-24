(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
  (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll)) (first coll) (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
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
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (cond
    (zero? n) 0
    (zero? k) 1
    :else (* n (power n (- k 1)))
    ))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()
    ))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to ) (my-range (dec up-to)))
    '()
    ))

(defn tails [a-seq]
  (if (or (empty? a-seq) (nil? a-seq))
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (tails a-seq)))



(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a (first a-seq)
          nowCount (or (get freqs a) 0)
          updateCount (inc nowCount)
          updateFreqs (assoc freqs a updateCount)]
      (my-frequencies-helper updateFreqs (rest a-seq))
      )
    ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) ()
    (let [a (first a-map)
          pair (repeat (val a) (key a))]
      (concat pair (un-frequencies (rest a-map)))
      )))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll)) ()
    (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (<= n 0) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (if (empty? a-seq)
      []
      [(my-take half a-seq) (my-drop half a-seq)]
    )))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    ))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[m-seq n-seq] (halve a-seq)]
    (seq-merge (merge-sort m-seq) (merge-sort n-seq))
    )))

(defn inits [a-seq]
   (reverse (map reverse (tails (reverse a-seq)))))

(defn split-into-monotonics [a-seq]
 (if (empty? a-seq) '()
   (let [monotonic? (fn [seq] (or (apply <= seq) (apply >= seq)))
         long-mon-seq (first (reverse (take-while monotonic? (rest (inits a-seq)))))]
     (cons long-mon-seq (split-into-monotonics (drop (count long-mon-seq) a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (partition (count a-seq) 1 (rest (concat a-seq a-seq)))
    ))

(defn permuCount [n]
  (* n (permuCount (dec n))))

(defn permutations [a-set]
  (if (empty? a-set) [()]
    (apply concat (map
                    (fn [a] (map cons (repeat (first a)) (permutations (rest a))))
                    (rotations a-set)))))

(defn powerset [a-set]
  (let [a (set a-set)]
    (set (conj (mapcat (fn [n] (powerset (disj a n))) a) a))))

