(ns recursion)

(defn product [coll]
  (if (empty? coll) 
    1
    (* (first coll) (product (rest coll)))))

; (product [1 2 4])
; (* 1 (product [2 4]))
; (* 1 (* 2 (product [4])))
; (* 1 (* 2 (* 4 (product []))))
; (* 1 (* 2 (* 4 1)))
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (if (empty? coll) 
    false 
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll) 
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (not (== (count a-seq) (count b-seq)))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1)
      seq-1
    (empty? seq-2)
      seq-2
    :else
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

; (cons (+ 1 4) (cons (+ 2 4) (cons (+ 3 4) '()))))
; (cons (+ 1 4) (cons (+ 2 4) (cons 7 '()))))
; (cons (+ 1 4) (cons (+ 2 4) (7)))
; (cons (+ 1 4) (cons 6 (7)))
; (cons (+ 1 4) (6 7))
; (cons 5 (6 7))
; (5 6 7)

(defn power "Computer n to the power of k." [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib "Computes the nth Fibonacci number." [n]
  (cond
    (== n 0)
      0
    (== n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat "Generates a list of what-to-repeat how-many-times number of times." [how-many-times what-to-repeat]
  (cond 
    (neg? how-many-times)
      ()
    (zero? how-many-times)
      ()
    :else
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
;(cons :a (cons :a ()))
;(cons :a (:a))
;(:a :a)

(defn my-range "Returns a list of numbers containing ]up-to, 0]." [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [result (map concat (tails a-seq) (inits a-seq))]
    (if (> (count result) 1)
      (rest result)
      result)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          newmap (if (contains? freqs elem)
                    (assoc freqs elem (+ 1 (get freqs elem)))
                    (assoc freqs elem 1))]
      (my-frequencies-helper newmap (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[fst snd] (first a-map)]
      (concat (repeat snd fst) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond 
    (== 0 n)
      (empty coll)
    (empty? coll)
      coll
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (== 0 n)
      coll
    (> n (count coll))
      (empty coll)
    :else
      (my-drop (dec n) (rest coll))))

(defn halve "Returns a vector with the sequence halved." [a-seq]
  (let [split (int (/ (count a-seq) 2))
        fst (my-take split a-seq)
        snd (my-drop split a-seq)]
    [fst snd]))

(defn seq-merge "Merges two sorted (low-to-high) sequences." [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort "Implementation of merge-sort." [a-seq]
  (cond
    (<= (count a-seq) 1)
      a-seq
    :else
      (seq-merge (merge-sort ((halve a-seq) 0)) (merge-sort ((halve a-seq) 1)))))
;(seq-merge (merge-sort [4 3]) (merge-sort [2 1]))
;(seq-merge (seq-merge (merge-sort (4)) (merge-sort (3))) (seq-merge (merge-sort (2)) (merge-sort (1))))
;(seq-merge (seq-merge (4) (3)) (seq-merge (2) (1)))
;(seq-merge (3 4) (1 2))
;(1 2 3 4)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

