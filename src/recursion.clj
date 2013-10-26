(ns recursion)

(defn product [coll]
  "Product is the first element * product of the rest"
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;Exercise2 ??
;(cons 1
;      (cons 2
;            (cons 4 '())))
; product is 1 if the seq is empty, or,
; first element * product of the rest of the elements
;(product [1 2 4])
;(product (cons 1(cons 2 (cons 4 '())) ))
;(* 1 (product (cons 2 (cons 4 '()))))
;(* 1 (* 2 (product (cons 4 '()))))
;(* 1 (* 2 (* 4 (product '()))))
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
;8

(defn singleton? [coll]
  "Empty list is false. If rest of the list is empty, the list is false too."
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  "Prints the current item when the rest of the list is empty"
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  "The max element is the max(first, max of the rest of the elements)"
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max(first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  "Max seq has higher count"
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  "Longer of the first sequence and the longest of the rest of the sequences"
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  "Add the first element that pred? element returns true,
  and any fitting elements from the rest of the sequence."
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  "If the first one is a match, return it.
  Otherwise check the rest of the sequence"
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  "Add an element that is a match. Then add any subsequent items that match."
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  "If first is true, drop it. Continue for the rest."
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  "If both are empty, true. If the first elements are not same, return false.
  Else continue with the rest of the sequences"
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (not(empty? b-seq))) false
   (and (empty? b-seq) (not(empty? a-seq))) false
   (and (not(== (first a-seq) (first b-seq)))) false
   :else (seq= (rest a-seq) (rest b-seq))))

 (defn my-map [f seq-1 seq-2]
   "Construct a sequence whose first value is f( first seq-1, first seq-2),
   and second value is the f( first of rest seq-1, first of rest seq-2)"
   (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  "n^0 = 1, n^1 = n, n * n^(k-1)"
  (cond
   (== k 0) 1
   (== k 1) n
   :else (* n (power n (dec k)))))


(defn fib [n]
  "F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)"
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib(- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  "Cons what-to-repeat with itself and decrease how-many-times.
  Repeat until how-many-times <= 1"
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  "Cons up-to-1 with my range [dec up-to]. Repeat until up-to 1."
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  "Cons a list of all the values from a-seq with the rest of the lists
  containing rest of the values of a-seq. Continue until a-seq empty."
  (if (empty? a-seq)
    '(())
    (cons (map (fn [x] x) a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  "Cons a list of all the values from a-seq with the rest of the lists
  created from a-seq from which the last value is dropped"
   (if (empty? a-seq)
     '(())
      (cons (map (fn [x] x) a-seq)
            (inits (butlast a-seq)))))

(defn rotations-helper2 [b-seq n]
  (if (= 0 n)
    '()
    (cons (first b-seq)
          (rotations-helper2 (rest b-seq) (dec n)))))

(defn rotations-helper1 [ rot n b-seq]
  (if (= rot n)
    '()
    (cons (rotations-helper2 b-seq n)
          (rotations-helper1 (inc rot) n (rest b-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper1 0 (count a-seq) (concat a-seq a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs
                               (first a-seq)(inc (get freqs (first a-seq))))
                             (rest a-seq))

      (my-frequencies-helper (assoc freqs
                               (first a-seq) 1)
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
(if (empty? a-map)
  ()
  (concat (repeat (val (first a-map)) (key (first a-map)))
          (un-frequencies (rest a-map)))))


(defn my-take [n coll]
  "Cons first and the rest of the values until n reaches zero or coll becomes empty"
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  "If n > 0, drop the first item.
   If n<=0, cons the first item with the rest of the items."
  (cond
   (empty? coll) '()
   (> n 0) (my-drop (dec n) (rest coll))
   :else (cons (first coll)
               (my-drop n (rest coll)))))

(defn halve [a-seq]
  "Conjoin my-take and my-drop, n = length/2"
  (let [n (int (/ (count a-seq) 2))]
    (conj '[] (my-take n a-seq) (my-drop n a-seq))))

(defn seq-merge [a-seq b-seq]
  "Start cons with the min(a-seq, b-seq),
  and continue with the rest of the mins"
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq)
      (first b-seq)) (cons (first a-seq)
                           (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq)
               (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [halfs (halve a-seq)
        [x] halfs
        [_ y] halfs]
      (seq-merge (merge-sort x)
                 (merge-sort y)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

