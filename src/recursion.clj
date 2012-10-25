(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

; (product [1 2 4]
; (product (cons 1 (cons 2 (cons 4 []))))
;=> (* 1 (product (cons 2 (cons 4 []))))
;=> (* 1 (* 2 (product (cons 4 []))))
;=> (* 1 (* 2 (* 4 (product []))))
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
    (empty? seq-1) seq-2
    (empty? seq-2) seq-1
    (= (rest seq-2) (seq-max (rest seq-1) (rest seq-2))) seq-2
    :else seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
   (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-elem (first a-seq) others (rest a-seq)]
      (if (pred? first-elem)
        (cons first-elem (my-filter pred? others))
        (my-filter pred? others)))))
    
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))


(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))


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

