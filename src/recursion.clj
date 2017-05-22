(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll)(product (rest coll)))))

(defn singleton? [coll]
  (not (or (empty? coll)(not(empty?(rest coll))))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)(max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max(first a-seq)(longest-sequence(rest a-seq))))))

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
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
   (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))(fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (or (= what-to-repeat nil) (> 0 how-many-times) (= 0 how-many-times)) ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to) ()
    (cons  (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (inits (drop-last a-seq)))))

(defn rotations-helper [n a-seq]
  (if (= n 0)
    ()
    (cons (seq a-seq) (rotations-helper (dec n) (concat (rest a-seq) (list (first a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    (def my-map {})
    (def my-map {(first a-seq) 1}))
  (if (empty? a-seq)
  my-map
  (my-frequencies-helper my-map (rest a-seq))))

(defn un-frequencies [a-map]
  (let [keys (keys a-map)
        vals (vals a-map)]
  (if (empty? a-map)
  ()
  (concat (repeat (first vals) (first keys)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    ()
    (cons (first coll)(my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (let [x (- (count coll) n)
        my-coll (reverse coll)
        kaka (if (< x 0) 0 x)]
  (reverse (my-take kaka my-coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
     (cons (my-take half a-seq)(list (my-drop half a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    (def my-map {})
    (def my-map {(first a-seq) 1}))
  (if (empty? a-seq)
  my-map
  (my-frequencies-helper my-map (rest a-seq))))

(defn seq-merge-helper [merg a-seq b-seq]
  (if (and (empty? a-seq)(empty? b-seq))
    (reverse merg)
    (if (empty? a-seq)
      (seq-merge-helper (cons (first b-seq) merg) a-seq (rest b-seq))
      (if (empty? b-seq)
        (seq-merge-helper (cons (first a-seq) merg) (rest a-seq) b-seq)
        (if (< (first a-seq) (first b-seq))
          (seq-merge-helper (cons (first a-seq) merg) (rest a-seq) b-seq)
          (seq-merge-helper (cons (first b-seq) merg) a-seq (rest b-seq)))))))

(defn seq-merge [a-seq b-seq]
  (if (< (first a-seq) (first b-seq))
    (def merg (cons (first a-seq) []))
    (def merg (cons (first b-seq) [])))
    (if (< (first a-seq) (first b-seq))
      (seq-merge-helper merg (rest a-seq) b-seq)
      (seq-merge-helper merg a-seq (rest b-seq))))

(defn merge-sort-helper [n a-seq]
  (if (or (= n 0)(= n 1))
    a-seq
    (let [[eka toka] (halve a-seq)]
      (seq-merge
        (merge-sort-helper (count eka) eka)
        (merge-sort-helper (count toka) toka)))))

(defn merge-sort [a-seq]
  (merge-sort-helper (count a-seq) a-seq))

(defn split-into-monotonics [a-seq]
  )

(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 5 4 7 1 3])
(inits [1 2 3 4]);=> ((0 5) (4 7) (1 3))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

