(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product(rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll)))
       (empty? (next coll))))

(defn my-last [coll]
  (if (empty? (next coll))
    (first coll)
    (my-last (next coll))))

(defn max-element [a-seq]
  (loop [coll (next a-seq), acc (first a-seq)]
    (if (nil? (first coll))
      acc
      (recur (next coll) (max acc (first coll))))))

(defn seq-max-old [seq-1 seq-2]
    (if (< (count seq-1) (count seq-2))
         seq-2
         seq-1))

(defn seq-max [s1 s2]
  (loop [r1 s1, r2 s2]
    (cond
     (empty? r2) s1
     (empty? r1) s2
     :else (recur (next r1) (next r2)))))

(defn longest-sequence [a-seq]
  (loop [acc-seq nil, colls a-seq]
    (if (empty? colls)
      acc-seq
      (recur (seq-max acc-seq (first colls)) (next colls)))))

(defn my-filter [pred? a-seq]
   (letfn [(helper [coll]
     (cond (empty? coll)
           '()
           (pred? (first coll))
           (cons (first coll) (helper (next coll)))
           :else (helper (next coll))))]
     (helper a-seq)))

(defn sequence-contains? [elem a-seq]
  (loop [coll a-seq]
    (cond (empty? coll) false
          (= elem (first coll)) true
          :else (recur (next coll)))))

(defn my-take-while [pred? a-seq]
  (letfn [(helper [coll]
                  (cond (empty? coll) '()
                        (not (pred? (first coll))) '()
                        :else (cons (first coll) (helper (next coll)))))]
    (helper a-seq)))

(defn my-drop-while [pred? a-seq]
  (letfn [(helper [coll]
                  (cond (empty? coll) '()
                        (not (pred? (first coll))) coll
                        :else (helper (next coll))))]
    (helper a-seq)))

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

