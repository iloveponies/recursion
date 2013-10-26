(ns recursion)

(defn product [coll]
 (if (empty? coll)
    1
      (* (first coll)
         (product (rest coll)))))


(product [1 2 4]) ;=> 8
; (* 1 (product [2 4]))
; (* 1 (* 2 (product [4])))
; (* 1 (* 2 (* 4 (product []))))
; (* 1 (* 2 (* 4 1))) ; [] = 1
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (if (or (empty? coll) (not (empty? (rest coll))))
    false
      true))


(defn my-last [coll]
  (cond
    (empty? coll) nil
    (and (not (= (first coll) nil)) (empty? (rest coll))) (first coll)
    :else (my-last (rest coll))))


(defn max-element [a-seq]
   (if(empty? a-seq)
     nil
       (max (first a-seq)
              (if(empty? (rest a-seq))
                (first a-seq)
                  (max-element (rest a-seq))))))


(defn seq-max [seq-1 seq-2]
  (let [seqq (fn [x y]
   (cond
    (empty? (rest x)) 2
    (empty? (rest y)) 1
    :else (recur (rest x) (rest y))
   ))]
     (if (= 1 (seqq seq-1 seq-2))
            seq-1
              seq-2)))
; X_X

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
      (if (singleton? a-seq)
        (first a-seq)
          (seq-max (first a-seq) (longest-sequence (rest a-seq))))))
; ^_^

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
   (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) (cons (first a-seq) (rest a-seq) )
    :else (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
    (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
    (zero? k) 1
    (zero? n) 0
    (= 1 k) n
    :else (* n (power n (dec k)))))


(defn fib [n]
 (cond
  (= n 0) 0
  (= n 1) 1
  :else (+ (fib (dec n)) (fib (dec (dec n))))))


(defn my-repeat [how-many-times what-to-repeat]
   (cond
    (> 0 how-many-times) (list)
    (= how-many-times 0) what-to-repeat
    (list? what-to-repeat) (cons (first what-to-repeat) (my-repeat (dec how-many-times) what-to-repeat))
    :else (my-repeat (dec how-many-times) (list what-to-repeat))))


(defn my-range [up-to]
  (cond
    (= up-to 0) (list)
    (< 0 up-to) (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (cond
  (empty? a-seq) '(())
  :else (cons (map merge a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (map sort (reverse (tails (reverse a-seq)))))


(defn rotatitons-helper [sq tmp]
  (cond
   (empty? tmp) '()
   :else (cons (take (count sq) (concat tmp sq) ) (rotatitons-helper sq (rest tmp)))))


(defn rotations [a-seq]
  (cons (map merge a-seq) (rotatitons-helper a-seq (rest a-seq))))

(defn my-frequencies-helper [freqs a-seq]
 )

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

