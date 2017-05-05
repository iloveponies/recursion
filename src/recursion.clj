(ns recursion)

(defn product [coll]
  ;; more concise
  ;;   (reduce (fn [a b] (* a b)) 1 coll))
  (if (empty? coll)
    1
    (let [a (first coll)
          rest (rest coll)]
      (if rest
        (* a (product rest))
        a))))

(defn singleton? [coll]
  ;; more complexity just for the sake of reduce
  ;; (simpler would be better)
  ;; (= 1 (reduce (fn [a b] (inc a)) 0 coll))
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  ;; using loop-recur
  ;; (loop [[val & rest] coll]
  ;;  (if rest
  ;;    (recur rest)
  ;;    val))
  (let [[a & rest] coll]
    (if rest
      (my-last rest)
      a)))

(defn max-element [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (max a (max-element rest))
      a)))

(defn seq-max [seq-1 seq-2]
  (first (sort-by count #(>= %1 %2) [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (seq-max a (longest-sequence rest))
      a)))

(defn my-filter [pred? a-seq]
  (into (empty a-seq)
        (let [a (first a-seq)
              rest (rest a-seq)]
          (if (empty? rest)
            (if (pred? a) (list a) '())
            (if (pred? a)
              (cons a (my-filter pred? rest))
              (my-filter pred? rest))))))

(defn sequence-contains? [elem a-seq]
  :-)

(defn my-take-while [pred? a-seq]
  [:-])

(defn my-drop-while [pred? a-seq]
  [:-])

(defn seq= [a-seq b-seq]
  :-)

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

