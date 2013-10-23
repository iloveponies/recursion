(ns recursion)

(defn product [coll]
  (if(empty? coll)
    1
    (* (first coll) (product (rest coll)))))

  ;(product [])
  ;(product [1 2 3])
  ;(product [1 2 3 4])

(defn singleton? [coll]
  (if(empty? coll)false
    (empty? (rest coll))))

;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (cond
   (empty? coll)nil
   (singleton? coll)(first coll)
   :else (my-last(rest coll))))

;(my-last [])      ;=> nil
;(my-last [1 2 3]) ;=> 3


(defn max-element [a-seq]
  (cond
   (empty? a-seq)nil
   (singleton? a-seq)(first a-seq)
   :else (max-element (cons (max (first a-seq)(second a-seq))(drop 2 a-seq)))

   ))

;(max-element [2 4 1 4]) ;=> 4
;(max-element [2])       ;=> 2
;(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if(> (count seq-1) (count seq-2))seq-1 seq-2))

;(seq-max [1] [1 2])   ;=> [1 2]
;(seq-max [1 2] [3 4 5]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq)(first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))

   ))

;(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;(longest-sequence [[1 2]])            ;=> [1 2]
;(longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
   (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
            (my-filter pred? (rest a-seq)))))

;(my-filter odd? [1 2 3 4]) ;=> (1 3)
;:(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;(my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)false
   (singleton? a-seq)(= (first a-seq) elem)
   :else (sequence-contains? elem (rest a-seq))
   ))

;(sequence-contains? 3 [1 2 3]) ;=> true
;(sequence-contains? 3 [4 7 9]) ;=> false
;(sequence-contains? :pony [])

(defn my-take-while [pred? a-seq]
   (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

;(my-take-while odd? [1 2 3 4])  ;=> (1)
;(my-take-while odd? [1 3 4 5])  ;=> (1 3)
;(my-take-while even? [1 3 4 5]) ;=> ()
;(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons (first a-seq)(rest a-seq))))

;(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;(my-drop-while odd? [])         ;=> ()


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (not(empty? a-seq)) (empty? b-seq)) false
    (and (not(empty? b-seq)) (empty? a-seq)) false
    (= (first a-seq) (first b-seq))(seq= (rest a-seq) (rest b-seq))
    :else false))

  ;(seq= [1 2 4] '(1 2 4))  ;=> true
  ;(seq= [] [])             ;=> true
  ;(seq= [1 2 nil] [1 2])   ;=> false
  ;(seq= [1 4 2] [1 2 4])   ;=> false
  ;(seq= [1 2 3] [1 2 3 4]) ;=> false
  ;(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

;(my-map + [1 2 3] [4 4 4]) ;=> (5 6 7)
;(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;(my-map + [1 2 3] [])        ;=> ()


(defn power [n k]
  (if(zero? k)
    1
    (* n (power n (dec k)))))

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
          (+ (fib (- n 1))
             (fib (- n 2)) ))
   )

;(fib 0) ;=> 0
;(fib 1) ;=> 1
;(fib 2) ;=> 1
; (fib 3) ;=> 2
; (fib 4) ;=> 3
; (fib 5) ;=> 5
; (fib 6) ;=> 8

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

;(my-repeat 2 :a)    ;=> (:a :a)
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (cond
   (== up-to 0) '()
   :else (cons (- up-to 1) (my-range (dec up-to)))
    ))

;(my-range 0)  ;=> ()
;(my-range 1)  ;=> (0)
;(my-range 2)  ;=> (1 0)
;(my-range 3)  ;=> (2 1 0)

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

