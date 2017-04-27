(ns recursion)

(defn product [coll]
  (reduce (fn [a b] (* a b)) 1 coll))

(defn singleton? [coll]
  (= 1 (reduce (fn [a b] (inc a)) 0 coll)))

(defn my-last [coll]
  (loop [[val & rest] coll]
    (if rest
      (recur rest)
      val)))

(defn max-element [a-seq]
  (loop [[val & rest] a-seq
         highest val]
    (if rest
      (if (> val highest)
        (recur rest val)
        (recur rest highest))
      highest)))

(defn seq-max [seq-1 seq-2]
  (first (sort-by count #(>= %1 %2) [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (loop [[val & rest] a-seq
         longest val]
    (if rest
      (recur rest (seq-max val longest))
      longest)))

(defn my-filter [pred? a-seq])

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

