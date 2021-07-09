(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
  (let [loppu (rest coll)
      loppuTyhja(empty? loppu)
    ]loppuTyhja)
))

(defn my-last [coll]
  (first (take-last 1 coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
  (max (first a-seq) (my-last a-seq))))


(defn seq-max [seq-1 seq-2]
  (let [eka (count seq-1)
        toka (count seq-2)
        returnValue (if(> eka toka)
                      seq-1
                      seq-2)]returnValue))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [summa(map (fn[x] (count x)) a-seq)
         maksimi(apply max summa)
          indeksi(.indexOf (max summa) maksimi)
        returnValue (get a-seq indeksi)
          ]returnValue)
    ))

(defn my-filter [pred? a-seq]
  (filter pred? a-seq))

(defn sequence-contains? [elem a-seq]
  (contains? (set a-seq) elem))

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

