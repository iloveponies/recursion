(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
 (and (not (empty? coll)) (not (get (into [] (sort coll)) 1))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
  (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
   (if (pred? (first a-seq))
       (cons (first a-seq) (my-filter pred? (rest a-seq)))
       (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not (= elem (first a-seq)))
     (sequence-contains? elem (rest a-seq))
   :else
     true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   ()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
    ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   ()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
    (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (not (= (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
 (if (< n 2)
   n
   (+ (fib (- n 1))
      (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [[x y] [how-many-times what-to-repeat]]
  (if (< x 1)
    ()
    (concat [y] (my-repeat (- x 1) y)))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (conj a-seq ())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
 (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper [n a-seq]
  (if (= n 0)
    ()
    (let [new-count (dec n)]
    (cons (seq a-seq) (rotations-helper new-count
                      (concat (rest a-seq) [(first a-seq)]))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
  (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
   (if (contains? freqs (first a-seq))
     (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
     (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
  (concat (repeat (second (first a-map)) (first (first a-map)))
          (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    ()
   (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
     coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
   [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge-helper [a-seq b-seq r]
  (if (or (empty? a-seq) (empty? b-seq))
   (concat r a-seq b-seq)
    (if (<= (first a-seq) (first b-seq))
      (seq-merge-helper (rest a-seq) b-seq  (conj r (first a-seq)))
      (seq-merge-helper a-seq (rest b-seq) (conj r (first b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    ()
  (if (<= (count a-seq) 1)
   a-seq
   (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq)))))))

(defn split-into-monotonics [a-seq]
 (let [monotonic? (fn [a-seq] (or
                               (apply <= a-seq)
                               (apply >= a-seq)))
       take-mono (fn [a-seq] (last (my-take-while monotonic? (rest (inits a-seq)))))]
   (if (empty? a-seq)
     ()
     (cons (take-mono a-seq)
           (split-into-monotonics (drop (count (take-mono a-seq)) a-seq))))))

(defn permutations [a-set]
  (let [iter-perm (fn [[f & r]]
                  (map (fn [x] (cons f x)) (permutations r)))]
    (if (empty? a-set)
      (cons a-set a-set)
      (apply concat (map iter-perm (rotations a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (hash-set (hash-set))
   (clojure.set/union (powerset (rest a-set))
                      (map #(conj % (first a-set)) (powerset (rest a-set))))))
