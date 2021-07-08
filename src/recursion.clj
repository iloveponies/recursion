(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [fst (first a-seq)
            rst (rest a-seq)]
      (if (singleton? a-seq)
        fst
        (max fst (max-element rst))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [fst (first a-seq)
            rst (rest a-seq)]
      (if (singleton? a-seq)
        fst
        (seq-max fst (longest-sequence rst))))))

(defn my-filter [pred? a-seq]
  (letfn [(f [o-seq f-seq]
               (if (empty? o-seq)
                 f-seq
                 (if (pred? (first o-seq))
                   (f (rest o-seq) (concat f-seq [(first o-seq)]))
                   (f (rest o-seq) f-seq))))]
  (f a-seq ())))

(defn sequence-contains? [elem a-seq]
  (< 0 (count (my-filter #(= elem %) a-seq))))

(defn my-take-while [pred? a-seq]
  (letfn [(f [o-seq f-seq]
               (if (empty? o-seq)
                 f-seq
                 (if (pred? (first o-seq))
                   (f (rest o-seq) (concat f-seq [(first o-seq)]))
                   f-seq)))]
  (f a-seq ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (empty? a-seq)
    (empty? b-seq)
    (if (empty? b-seq)
      false
      (if (== (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn my-map [f seq-1 seq-2]
  (letfn [(fun [o-seq-1 o-seq-2 f-seq]
               (if (or (empty? o-seq-1) (empty? o-seq-2))
                 f-seq
                 (fun (rest o-seq-1) (rest o-seq-2) (concat f-seq [(f (first o-seq-1) (first o-seq-2))]))))]
  (fun seq-1 seq-2 ())))

(defn power [n k]
  (letfn [(fun [x nk]
               (if (= nk 0)
                 x
                 (fun (* x n) (- nk 1))))]
  (fun 1 k)))

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

