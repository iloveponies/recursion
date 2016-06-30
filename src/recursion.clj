(ns recursion)

(defn product [coll]
  (apply * coll))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (first (reverse coll)))

(defn max-element [a-seq]
  (last (sort a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (not (empty? a-seq))
    (->> (sort-by count a-seq)
         (last))))

(defn do-pred [pred? acc item]
  (if (pred? item)
    (conj acc item)
    acc))

(defn my-filter [pred? a-seq]
  (reduce #(do-pred pred? %1 %2) [] a-seq))

(defn sequence-contains? [elem a-seq]
  (->> (drop-while #(not (= elem %)) a-seq)
       (empty?)
       (not)))

(defn my-take-while
  "Accumulates until pred? is false.
   If no acc is provided, defaults to a vector."
  ([pred? a-seq]
   (my-take-while pred? [] a-seq))
  ([pred? acc a-seq]
   (let [[head & tail] a-seq]
     (cond
       (nil? head) acc
       (pred? head) (recur pred? (conj acc head) tail)
       (not (pred? head)) acc))))

(defn my-drop-while [pred? a-seq]
  (let [[head & tail] a-seq]
    (cond
      (nil? head) '()
      (pred? head) (recur pred? tail)
      (not (pred? head)) a-seq)))

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(defn- alive? [& heads]
  (->> heads
       (map nil?)
       (every? false?)))

(defn my-map
  ([f seq-1 seq-2]
    (my-map f [] seq-1 seq-2))
  ([f acc seq-1 seq-2]
    (let [[head1 & tail1] seq-1
          [head2 & tail2] seq-2]
      (cond
        (alive? head1 head2) (recur f (conj acc (f head1 head2)) tail1 tail2)
        :else acc))))

(defn power [n k]
  (apply * (repeat k n)))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat
  ([how-many-times what-to-repeat]
   (my-repeat [] how-many-times what-to-repeat))
  ([acc n item]
   (if (<= n 0)
     acc
     (recur (conj acc item) (dec n) item))))

(defn my-range
  ([up-to]
   (my-range [] (dec up-to)))
  ([acc up-to]
   (if (>= up-to 0)
     (recur (conj acc up-to) (dec up-to))
     acc)))

(defn tails
  ([a-seq]
   (tails [a-seq] a-seq))
  ([acc a-seq]
   (let [tail (rest a-seq)]
     (if (empty? a-seq)
       acc
       (recur (conj acc tail) tail)))))

(defn inits [a-seq]
  (->> a-seq
       (reverse)
       (tails)
       (map reverse)
       (reverse)))

(defn rotations
  ([a-seq]
   (rotations [a-seq] a-seq (rest a-seq)))
  ([acc a-seq remaining]
   (if (empty? remaining)
     acc
     (let [rotated (concat (rest a-seq) [(first a-seq)])]
       (recur (conj acc rotated) rotated (rest remaining))))))

(defn my-frequencies-helper [freqs a-seq]
  "no thanks")

(defn my-frequencies
  ([a-seq]
   (my-frequencies {} a-seq))
  ([acc [head & rems]]
   (cond
     (nil? head) acc
     (nil? (get acc head)) (recur (assoc acc head 1) rems)
     :else (recur (assoc acc head (inc (acc head))) rems))))

(defn un-frequencies [a-map]
  (->> a-map
       (map (fn [[a n]] (repeat n a)))
       (apply concat)))

(defn my-take
  ([n coll]
   (my-take [] n coll))
  ([acc n coll]
   (cond
     (= 0 n) acc
     (empty? coll) acc
     :else (recur (conj acc (first coll)) (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (= 0 n) coll
    (empty? coll) coll
    :else (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn- sorted-insert [item top bot]
  "Inserts item in its sorted position
   between top & bot parts of the destination seq"
  (let [[hd & tail] bot]
    (cond
      (nil? hd) (conj top item)
      (> item hd) (recur item (conj top hd) tail)
      :else (apply conj top item bot))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (recur
      (rest a-seq) (sorted-insert
                          (first a-seq) [] b-seq))))

(defn- do-merge-sort [coll]
  (if (singleton? coll)
    (first coll)
    (->> coll
         (partition 2 2 (repeat nil))
         (map #(seq-merge (first %) (second %)))
         (recur))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    []
    (->> a-seq
         (map #(conj nil %))
         (do-merge-sort))))

(defn split-into-monotonics
  ([a-seq]
   (split-into-monotonics [] a-seq))
  ([acc coll]
   (if (empty? coll)
     acc
     (as-> coll _
          (inits _)
          (rest _)  ; drop the first empty list
          (take-while #(or (apply <= %) (apply >= %)) _) ; take monotonic groups
          (last _)
          ;; using recur works fine from the repl (clj-1.8)...
          ;; ... but not lein midje which has clj-1.5 in its .dotfile-conf
          (split-into-monotonics (conj acc _) (drop (count _) coll))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

