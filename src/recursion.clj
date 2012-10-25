(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll) (first coll) (if (empty? coll) nil (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [rst (rest (rest a-seq))]
    (if (singleton? a-seq) (first a-seq) 
      (if (empty? a-seq) nil 
        (max-element (cons (max (first a-seq) (first (rest a-seq))) rst )))))) 

(defn seq-max [seq-1 seq-2]
  (let [fun (fn ! [x y] (if (empty? x) false
                        (if (empty? y) true (! (rest x) (rest y)))))]
    (if (fun seq-1 seq-2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (let [rst (rest (rest a-seq))]
    (if (singleton? a-seq) (first a-seq) 
      (if (empty? a-seq) nil 
        (longest-sequence (cons (seq-max (first a-seq) (first (rest a-seq))) rst )))))) 

(defn my-filter [pred? a-seq]
  (let [rst (rest a-seq)
        [a] a-seq] 
    (if (singleton? a-seq) 
      (if (pred? a) [a] []) 
      (if (pred? a) (cons a (my-filter pred? rst)) (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (let [rst (rest a-seq)
        [a] a-seq]
    (if (empty? a-seq) false 
      (if (== elem a) true (sequence-contains? elem rst)))))

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