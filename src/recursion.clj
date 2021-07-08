(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (let [sing (= (rest coll) ())]
    (and (not (empty? coll)) sing)))

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
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) (seq a-seq)
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (count a-seq) (count b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* (power n (dec k)) n)))

(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (- n 2)) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
      (rest (map concat (tails a-seq) (inits a-seq)))))

(defn counter-map [a-map n]
  (let [val (get a-map n)]
    (if val
      (assoc a-map n (inc val))
      (assoc a-map n 1))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (counter-map freqs (first a-seq))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [v a-map]
  (if (empty? a-map)
    v
    (let [[val n] (first a-map)
          new-v (concat v (repeat n val))]
      (un-frequencies-helper new-v (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (<= (count coll) n)
    coll
    (my-take n (butlast coll))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(take half a-seq) (drop half a-seq)]))

(defn seq-merge-helper [v a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) v
        (empty? a-seq) (concat v b-seq)
        (empty? b-seq) (concat v a-seq)
        (< (first a-seq) (first b-seq))
        (seq-merge-helper (conj v (first a-seq)) (rest a-seq) b-seq)
        :else (seq-merge-helper (conj v (first b-seq)) a-seq (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= (count a-seq) 1))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (last (halve a-seq))))))

(defn inits [a-seq]
  (for [n (range (inc (count a-seq)))]
    (take n a-seq)))

(defn both? [p q]
  (or (every? true? [p q])
      (every? false? [p q])))

(defn monotonic? [[f s t :as a-seq]]
  (cond
   (< (count a-seq) 3) true
   (both? (< f s) (< s t)) (recur (rest a-seq))
   :else false))

(defn monotonic-chunk [a-seq]
  (let [chunks (drop 2 (inits a-seq))]
    (last (take-while monotonic? chunks))))

(defn split-into-monotonics
  ([a-seq] (split-into-monotonics a-seq []))
  ([a-seq out]
   (if (empty? a-seq)
     out
     (let [chunk (monotonic-chunk a-seq)]
       (recur
        (drop (count chunk) a-seq)
        (conj out chunk))))))

(defn permutations [s]
  (let [a-set (set s)]
    (cond (empty? a-set) '(())
          (empty? (rest a-set)) (list (apply list a-set))
          :else (for [x a-set y (permutations (disj a-set x))]
                  (cons x y)))))

 (defn powerset [a-set]
   (if (empty? a-set)
     '(())
     (let [lower (powerset (rest a-set))]
       (concat (map #(cons (first a-set) %) lower) lower))))
