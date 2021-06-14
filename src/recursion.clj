(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;(product [1 2 4])
;(* 1 (product [2 4]))
;(* 1 (* 2 (product [4])))
;(* 1 (* 2 (* 4 (product []))))
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
;8


(defn singleton? [coll]
  (if (empty? coll) false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
                (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not (= (first a-seq) elem))
     (sequence-contains? elem (rest a-seq))
   :else
     true))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq)))) ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))


(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (not (= (count a-seq) (count b-seq)))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false
   ))

(defn my-map [f seq-1 seq-2]
  (cond
    (and (not (empty? seq-1)) (not (empty? seq-2)))
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   :else
     ()
   ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (- up-to 1) (my-range (dec up-to)))
    ()))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
     (empty? a-seq)
       '(())
     :else
       (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    {}
    ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

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

