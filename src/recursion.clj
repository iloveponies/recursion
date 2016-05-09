(ns recursion)

;; done
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product(rest coll)))))

;; done
(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

;; done
(defn my-last [coll]
  (if(empty? coll)
    nil
    (if (singleton? coll)
     coll
     (my-last (rest coll)))))


;; done
(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else
    (let [others (rest a-seq)]
      (let [winner (max (first a-seq) (first others))]
        (max-element (cons winner (rest others)))))))

;; done
(defn seq-max [seq-1 seq-2]
  ( let [[a b] [(count seq-1) (count seq-2)]]
    (if (> a b) seq-1 seq-2)))

;; done
(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else
    (let [others (rest a-seq)]
      (let [winner (seq-max (first a-seq) (first others))]
        (longest-sequence (cons winner (rest others)))))))

;; done
(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a (first a-seq)]
      (cond
        (pred? a) (cons a (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))))


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

