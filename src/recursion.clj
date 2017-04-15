(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;(product [1 2 4])
;(product '(1 2 4))
;= (product cons(1 (cons 2 (cons 4 '()))))
;=>(* 1 (cons 2 (cons 4 '())))
;=>(* 1 (* 2 (cons 4 '())))
;=>(* 1 (* 2 (* 4 '())))
;=>(* 1 (* 2 (* 4 1)))            ;(empty? '()) is true, so (product '()) ;=> 1
;=>(* 1 (* 2 4))
;=>(* 1 8)
;=>8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    (first coll)
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (defn max-element-helper [aseq]
    (let [hat (empty? aseq)]
      (if hat
        0
        (max (first aseq)
             (max-element-helper (rest aseq))))))
  (if (empty? a-seq)
    nil
    (max-element-helper a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (longest-sequence (conj
      (rest (rest a-seq))
      (seq-max
        (first a-seq) (first (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
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
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1) '()
   (empty? seq-2) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (not (pos? how-many-times))
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (not (pos? up-to))
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails-h [a-seq]
  (if (empty? a-seq)
    '([])
    (cons (concat a-seq) (tails-h (rest a-seq)))))

(defn tails [a-seq]
  (tails-h a-seq))

(defn inits [a-seq]
  (tails-h (reverse a-seq)))

(defn rotations-h [a-seq lst n]
  (cond
   (zero? n) lst
   :else (rotations-h (conj lst (rest a-seq) (first a-seq)) (conj lst (rest a-seq) (first a-seq)) (dec n))
   ))

(defn rotations [a-seq]
  (rotations-h a-seq [] (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          new-freqs (if (freqs key)
                      (assoc freqs key (inc (freqs key)))
                      (assoc freqs key 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map]
  (if (empty? a-map)
    nil
    (cons (my-repeat (last (first a-map)) (first (first a-map))) (un-frequencies-helper (rest a-map)))))

(defn un-frequencies [a-map]
  (apply concat (un-frequencies-helper a-map)))

(defn my-take [n coll]
  (cond
   (empty? coll) '()
   (<= n 0) '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (> n 0) (my-drop (dec n) (rest coll))
   :else (cons (first coll) (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [division (int (/ (count a-seq) 2))]
    (vector (my-take division a-seq) (my-drop division a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) (cons b (seq-merge a (rest b-seq)))
     (empty? b-seq) (cons a (seq-merge (rest a-seq) b))
     (<= a b) (cons a (seq-merge (rest a-seq) b-seq))
     (< b a) (cons b (seq-merge a-seq (rest b-seq)))
     :else nil)))

(defn merge-sort [a-seq]
  (let [half-1 (first (halve a-seq))
        half-2 (second (halve a-seq))]
    (cond
     (empty? a-seq) '()
     (> (count a-seq) 3) (seq-merge (merge-sort half-1) (merge-sort half-2))
     (> (count a-seq) 1) (seq-merge half-1 half-2))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])








