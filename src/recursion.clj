(ns recursion)

; lists are recursive structs
(defn sum [coll]
  (if (empty? coll)
    0
    (+ (first coll)
       (sum (rest coll)))))
;(sum [1 2 3 4]) ;=> 10

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

; general template for linear recursion over collections
;(defn eats-coll [coll]
;  (if (empty? coll)
;    ...
;    (... (first coll) ... (eats-coll (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

; ex: saving the list
(defn my-map-2 [f a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (f (first a-seq))
          (my-map-2 f (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

; ex: stopping before the end
(defn only-numbers? [coll]
  (cond
    (empty? coll)
      true
    (number? (first coll))
      (only-numbers? (rest coll))
    :else
      false))
;(only-numbers? [1 2 3 4])    ;=> true
;(only-numbers? [1 2 :D 3 4]) ;=> false

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (some (set [elem]) (seq a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [s (seq a-seq)]
    (cond
      (empty? s)
        '()
      (pred? (first s))
        (cons (first s) (my-take-while pred? (rest s)))
      :else
        '())))


(defn my-drop-while [pred? a-seq]
  (let [s (seq a-seq)]
    (cond
      (empty? s)
        '()
      (and (pred? (first a-seq)) s)
        (my-drop-while pred? (rest a-seq))
      :else
        s)))

; ex: recurse over many seqs
(defn first-in [val seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) 0
    (= (first seq-1) val) 1
    (= (first seq-2) val) 2
    :else (first-in val (rest seq-1) (rest seq-2))))

(defn seq= [a-seq b-seq]
  (let [a (seq a-seq)
        b (seq b-seq)]
    (cond

      (and (empty? a) (empty? b))
        true
      (or (empty? a) (empty? b))
        false
      (= (first a) (first b))
        (seq= (rest a) (rest b))
      :else
        false)))

(defn my-map [f seq-1 seq-2]
  (let [a (seq seq-1)
        b (seq seq-2)]
    (cond
      (or (empty? a) (empty? b))
        '()
      (and a b)
        (cons
          (f (first a) (first b)) (my-map f (rest a) (rest b))))))

; ex more two seq maps
(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))
;(indexed [:a :b :c]) ;=> ([0 :a] [1 :b] [2 :c])

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))
;(consecutives [:a :b :c]) ;=> ([:a :b] [:b :c])
;(consecutives [1 2 3 4])  ;=> ([1 2] [2 3] [3 4])


; ex recursion on numbers

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))
;(factorial 1)

; general template for recursion over natural numbers
;(defn eats-numbers [n]
;  (if (zero? n)
;    ...
;    (... n ... (eats-numbers (dec n)))))

(defn power [n k]
  (if (zero? n)
    0
    (if (zero? k)
      1
      (* n (power n (dec k))))))

; non linear recursion

; ex tree (hierarchical structure) recursion
(defn f [n]
  (if (< n 3)
    n
    (+      (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))
;(f 3)

(defn fib [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [s (seq a-seq)
        r (reverse s)]
    (if (empty? s)
      (cons '() s)
      (seq (set (cons s (cons (rest s) (tails (rest s))))
)))))

(defn inits [a-seq]
  (let [s (seq a-seq)
        r (reverse s)]
    (if (empty? s)
      (cons '() s)
      (seq
        (set
          (cons s (cons (reverse (rest r)) (inits (reverse (rest r))))
))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '()
    (cons a-seq (concat (rotations (rest a-seq)) [(first a-seq)]))
))

;(defn rotations [a-seq]
;  (let [s (seq a-seq)
;        r (rest s)]
;  (if (zero? (count s))
;      (cons s '())
;      false)))

;(cons a-seq (rotations (rest a-seq)))))

;(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
;(rotations [:a :b])   ;=> ((:b :a) (:a :b))
;(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;(count (rotations [6 5 8 9 2])) ;=> 5


; ex passing state
(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]

      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))



(defn my-frequencies-helper [freqs a-seq])

;(println "a" freqs a-seq)
;(println "b" (get (first a-seq) freqs))
;    (conj freqs (set (vector (first a-seq) 0)))

;    (println "cba" freq-coll)


;  (if (nil? (get (first a-seq) freqs))
;    (conj freqs {(first a-seq) 1})
;    (conj freqs {(first a-seq) (inc (get (first a-seq) freqs))})



  ;  (if (empty? a-seq)
;    {}
;    (let [freq-count (if (first a-seq)
;                       (if (nil? (get freqs (first a-seq)))
;      (println "tata" freq-count)
;      (my-frequencies-helper {(first a-seq) freq-count} (rest a-seq))
;    )
;)


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;(my-frequencies []) ;=> {}
;(my-frequencies [:a])


;(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

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

