(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;=============================================

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

;=============================================

(defn my-last [coll]
  (when (seq coll)
    (if (singleton? coll) (first coll) (my-last (rest coll)))))

;=============================================

(defn max-element [a-seq]
  (my-last (sort a-seq)))

;=============================================

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

;=============================================

(defn longest-sequence [a-seq]
  (let [a-sorted-seq (sort a-seq)]
    (when (seq a-sorted-seq)
      (if (singleton? a-sorted-seq)
        (first a-sorted-seq)
        (longest-sequence (rest a-sorted-seq))))))

;=============================================

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

;=============================================

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

;=============================================

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      [])))

;=============================================

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

;=============================================

(defn seq= [a-seq b-seq]
  (if (= a-seq b-seq)
    true
    (if (and (= (first a-seq) (first b-seq))
             (not-empty (rest a-seq))
             (not-empty (rest b-seq)))
      (seq= (rest a-seq) (rest b-seq))
      false)))

;=============================================

(defn my-map [f seq-1 seq-2]
  (if (and (not-empty seq-1)
           (not-empty seq-2))
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ()))

;=============================================

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

;=============================================

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

;=============================================

(defn my-repeat [how-many-times what-to-repeat]
  (if-not (<= how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()))

;=============================================

(defn my-range [up-to]
  (if-not (<= up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    ()))

;=============================================

(defn tails [a-seq]
  (if-not (empty? a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))
    '(())))

;=============================================

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

;=============================================

(defn rotations [a-seq]
  (set (map concat (tails a-seq) (inits a-seq))))

;=============================================

(defn my-frequencies-helper [freqs a-seq]
  (if-not (empty? a-seq)
    (let [current-count (get freqs (first a-seq))]
      (if (nil? current-count)
        (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
        (my-frequencies-helper (assoc freqs (first a-seq) (inc current-count)) (rest a-seq))))
    freqs))

;=============================================

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;=============================================

(defn un-frequencies [a-map]
  (if-not (empty? a-map)
    (flatten
      (cons (repeat (val (first a-map))
                    (key (first a-map)))
            (un-frequencies (rest a-map))))))

;=============================================

(defn my-take [n coll]
  (first (split-at n coll)))
;  (if (and (pos? n)
;           (seq coll))
;    (cons (first coll) (my-take (dec n) (rest coll)))))

;=============================================

(defn my-drop [n coll]
  (first (rest (split-at n coll))))
;  (if (and (pos? n)
;           (seq coll))
;    (my-drop (dec n) (rest coll))
;    coll))

;=============================================

(defn halve [a-seq]
  (vec (split-at (int (/ (count a-seq) 2)) a-seq)))

;=============================================

(defn seq-merge [a-seq b-seq]
  (if-not (and (empty? a-seq)
               (empty? b-seq))
    (let [a (first a-seq)
          b (first b-seq)]
      (cond
        (nil? a) (cons b (rest b-seq))
        (nil? b) (cons a (rest a-seq))
        (< b a) (cons b (seq-merge a-seq (rest b-seq)))
        (< a b) (cons a (seq-merge (rest a-seq) b-seq))
        :else (cons '(a b) (seq-merge (rest a-seq) (rest b-seq)))))))

;=============================================

(defn merge-sort [a-seq]
  (cond
    (zero? (count a-seq)) ()
    (== (count a-seq) 1) (cons (first a-seq) ())
    :else (let [halved (halve a-seq)]
            (seq-merge (sort (first halved)) (sort (second halved))))))

;=============================================

(defn split-into-monotonics [a-seq]
  :-)

(inits [0 1 2 1 0])
(inits [0 5 4 7 1 3])

(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()
(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

;=============================================

(defn permutations [a-set]
  [:-])

;=============================================

(defn powerset [a-set]
  [:-])

;=============================================

