(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
    ))

; the evaluation of (product [1 2 4])
; (product (cons 1 (cons 2 (cons 4 '()))))
;
;=> (* 1 (product (cons 2 (cons 4 '()))))
;=> (* 1 (* 2 (product (cons 4 '()))))
;=> (* 1 (* 2 (* 4 (product '()))))
;=> (* 1 (* 2 (* 4 1))    ; (empty? '()) is true, so (product '()) ;=> 1
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn monotonic? [a-seq]
  ;(println a-seq)
  (cond
   (empty? a-seq) true
   :else (or (apply <= a-seq) (apply >= a-seq))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
   (> (count seq-1) (count seq-2)) seq-1
   :else seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not= false (pred? (first a-seq))) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not= false (pred? (first a-seq))) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()
   ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not= false (pred? (first a-seq))) (my-drop-while pred? (rest a-seq))
   :else a-seq
   ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   ))

(defn power [n k]
  ; 5 3 ;=> 125
  (cond
   (= 1 k) n
   (= 0 k) 1
   (= 0 n) 0
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  ;(print "inits: ")
  ;(println a-seq)
  (if (empty? a-seq)
    '(())
    (cons (sequence a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [i a-seq]
  (if (< i 1)
    '()
    (cons (sequence a-seq)
          (rotations-helper (- i 1) (concat
                                     (vector (first (reverse a-seq)))
                                     (reverse (rest (reverse a-seq))))))))

(defn rotations [a-seq]
  (if (< (count a-seq) 1)
    '(())
    (rotations-helper (count a-seq) a-seq)
    ))

(defn mapify [a-map a-set]
  (if (empty? a-set)
    a-map
    (mapify (assoc a-map (first a-set) 0) (rest a-set))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (freqs (first a-seq)))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper (mapify {} (set a-seq)) a-seq))

(defn un-frequencies-helper [a-seq]
  (if (empty? a-seq)
   a-seq
   (concat (repeat (nth (rest (first a-seq)) 0) (first (first a-seq))) (un-frequencies-helper (rest a-seq)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper (concat a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (> n (count coll))
    '()
    (reverse (my-take n (reverse coll)))))

(defn halve [a-seq]
  (let [length (count a-seq)
        half (int (/ length 2))]
    (vec (cons (my-take half a-seq) (cons (my-drop (- length half) a-seq) '())))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (> (first a-seq) (first b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
   ))

(defn merge-sort [a-seq]
  (cond
   (= 1 (count a-seq)) a-seq
   (empty? a-seq) '()
   :else (seq-merge (merge-sort (nth (halve a-seq) 0)) (merge-sort (nth (halve a-seq) 1)))))

(defn split-into-monotonics1 [n a-seq]
  ;(print "a-seq: ")
  ;(println a-seq)
  ;(print "count a-seq: ")
  ;(println (count a-seq))
  ;(print "n: ")
  ;(println n)
  ;(print "nth init: ")
  ;(println (nth (reverse (inits a-seq)) n))
  (cond
   (empty? a-seq) '()
   (= 1 (count a-seq)) (cons a-seq '())
   (and (= n (count a-seq)) (monotonic? (nth (reverse (inits a-seq)) n))) (cons a-seq '())
   ; (reverse (inits [1 2 3 4])) => (() (1) (1 2) (1 2 3) (1 2 3 4))
   ; skip them until either seq is no longer monotonic or sequence is empty
   (monotonic? (nth (reverse (inits a-seq)) n)) (cons '() (split-into-monotonics1 (+ 1 n) a-seq))
   ; previous sequence was true                      cut the sequence from a-seq that was monotonic
   :else (cons (nth (reverse (inits a-seq)) (- n 1)) (split-into-monotonics1 1 (nthrest a-seq (count (nth (reverse (inits a-seq)) (- n 1))))))))

(defn split-into-monotonics [a-seq]
  (filter (fn [x] (not (empty? x))) (split-into-monotonics1 1 a-seq)))

(defn permutations3 [a-set i]
  (if (= i (- (count a-set) 1))
    (concat a-set)
    (let [v a-set] (for [j (range i (count v))]
                     (permutations3 (assoc v i (v j) j (v i)) (inc i))))))

(defn flatten-helper [n a-set]
  (if (empty? a-set)
    a-set
    (cons (take n a-set) (flatten-helper n (nthrest a-set n)))))

(defn custom-flatten [n a-set]
  (flatten-helper n (flatten a-set)))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (custom-flatten (count a-set) (permutations3 (vec a-set) 0))))

(defn powerset [a-set]
  [:-])

