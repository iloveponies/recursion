(ns recursion)

(defn product [coll]
  (cond (empty? coll) 1
        :else (* (first coll) 
                 (product (rest coll)))))

; (product [1 2 3])
; = (product (cons 1 (cons 2 (cons 3 (cons '())))))
; => (* 1 (product (cons 2 (cons 3 (cons '())))))
; => (* 1 (* 2 (product (cons 3 (cons '())))))
; => (* 1 (* 2 (* 3 (product '()))))
; => (* 1 (* 2 (* 3 (* 1))))
; => (* 1 (* 2 (* 3 1)))
; => (* 1 (* 2 (* 3)))
; => (* 1 (* 2 3))
; => (* 1 6)
; => 6

(defn singleton? [coll]
  ;(and (empty? (rest coll)))
  ;     (true? (first coll)))
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  ;((comp first reverse) coll))
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
    (> (count seq-1) (count seq-2)) seq-1
    :else seq-2))
; it was taking way too much time for me to solve this using recursion.
; here's a little simplification of the route i was trying to take 
; before the return values drove me crazy.
;
;  (cond
;    (or (singleton? seq-1) (empty? seq-1)) seq-2 
;    (or (singleton? seq-2) (empty? seq-2)) seq-1 
;    :else 
;      (seq-max (rest seq-1) 
;               (rest seq-2))))



(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) 
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true 
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    ((comp not pred?) (first a-seq)) (cons (first a-seq) (rest a-seq))
    :else ()))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  ;why not zipWith?
  ;_______; ran out of time ;_____;
  (cond
    (or (empty? seq-1) (empty? seq-2)) () 
    :else (cons (f (seq-1) (seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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

