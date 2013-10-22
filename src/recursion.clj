(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

;; TODO fiksaa rekursio
(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (coll 0)
    :else (my-last (rest coll))))
  ;; (if (empty? coll)
  ;;   nil
  ;;   (if (singleton? coll)
  ;;     (coll 0)
  ;;     (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (cond
      (== len1 len2) seq-2
      (< len1 len2) seq-2
      :else seq-1)))

;; TODO: get rid of sort-by
(defn longest-sequence [s]
  (let [len (count s)]
    (cond 
      (== 0 len) nil
      (== 1 len) (first s)
      :else (last (sort-by count s)))))

(defn my-filter [pred? a-seq]
  ;; if the pred? meets the conditions add the elem to return value
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [a (count a-seq)
        b (count b-seq)]
    (cond
      (and (== 0 a) (== 0 b)) true
      (== 1 a b) (= (first a-seq) (first b-seq))
      (not (== (first a-seq) (first b-seq))) false
      :else (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  [:-])

(defn power [n k]
  :-)

(defn fib [n]
  ;; (cond
  ;;   (== 0) 0
  ;;   (< 0 n 3) 1
  ;;   :else (+ (fib (- n 2)) (fib (- n 2)))))
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

