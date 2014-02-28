(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

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
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not= (count a-seq) (count b-seq)) false
   (empty? a-seq) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper [n a-seq]
  (if (= 0 n)
    []
    (let [beginning (take n a-seq)
          end (drop n a-seq)
          new-rot (concat end beginning)]
      (cons new-rot (rotations-helper (dec n) a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freqs (assoc freqs elem (inc (get freqs elem 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[k v] (first a-map)
          elem (repeat v k)]
      (concat elem (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) []
   (zero? n) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (count a-seq)
        half (int (/ n 2))]
    [(take half a-seq) (drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a (first a-seq)
               b (first b-seq)]
           (if (< a b)
             (cons a (seq-merge (rest a-seq) b-seq))
             (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [[fh sh] (halve a-seq)
          sfh (merge-sort fh)
          ssh (merge-sort sh)]
      (seq-merge sfh ssh))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [i (drop 1 (inits a-seq))
          asc (take-while #(apply < %) i)
          desc (take-while #(apply > %) i)
          nasc (count asc)
          ndesc (count desc)]
      (if (> nasc ndesc)
        (cons (last asc) (split-into-monotonics (drop nasc a-seq)))
        (cons (last desc) (split-into-monotonics (drop ndesc a-seq)))))))

(defn permutations-helper [locked coll]
  (if (empty? coll)
    locked
    (let [rots (rotations coll)
          f (fn [r] (map #(cons (first r) %) (permutations-helper (conj locked (first r)) (rest r))))]
      (map f rots))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (permutations-helper [] a-set)))

(defn powerset [a-set]
  [:-])

