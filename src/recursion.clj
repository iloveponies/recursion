
(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; The if acts as our base case, otherwise the function
; will recursively call itself.

;(product [])        ;=> 1  ; special case
;(product [1 2 3])   ;=> 6
;(product [1 2 3 4]) ;=> 24
;(product [0 1 2])   ;=> 0
;(product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (if (not (nil? (first coll))) (nil? (first (rest coll))) false) )

;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (if (not (nil? (first (rest coll)))) (my-last (rest coll)) (first coll)))

;(my-last [])      ;=> nil
;(my-last [1 2 3]) ;=> 3
;(my-last [2 5])   ;=> 5

(defn max-element [a-seq]
(if (not (or (singleton? a-seq) (empty? a-seq)))
  (if (< (first a-seq) (last a-seq)) ;truethsy for outer if
    (max-element (rest a-seq))
    (max-element (butlast a-seq)))
  (if (empty? a-seq) nil (first a-seq)))) ;falsy for outer if

;(max-element [2 4 1 4]) ;=> 4
;(max-element [2])       ;=> 2
;(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

;(seq-max [1] [1 2])   ;=> [1 2]
;(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (if (not (or (singleton? a-seq) (empty? a-seq))) (seq-max (first a-seq) (longest-sequence (rest a-seq))) (first a-seq)))

;(longest-sequence [[1 2] [] [1 2 3]])
;(longest-sequence [[1 2]])            ;=> [1 2]
;(longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq ;truthy outer
    (if (pred? (first a-seq)) (cons (first a-seq)
     (my-filter pred? (rest a-seq))) ;truthy
     (my-filter pred? (rest a-seq)) ) ;falsey
    ))

;(my-filter odd? [1 2 3 4]) ;=> (1 3)
;(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;(my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

;(sequence-contains? 3 [1 2 3]) ;=> true
;(sequence-contains? 3 [4 7 9]) ;=> false
;(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
  (empty? a-seq) a-seq
  (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  :else '()))

;(my-take-while odd? [1 2 3 4])  ;=> (1)
;(my-take-while odd? [1 3 4 5])  ;=> (1 3)
;(my-take-while even? [1 3 4 5]) ;=> ()
;(my-take-while odd? [])         ;=> ()

;(cond
;  (durp) true false
;  (derp) false
;  :else false)

(defn my-drop-while [pred? a-seq]
(cond
  (empty? a-seq) '()
  (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
  :else a-seq))

;(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;(my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? a-seq)) (= (first a-seq) (first b-seq))
   (or (empty? a-seq) (empty? b-seq)) false
   (and (empty? (rest a-seq)) (empty? (rest b-seq)) ) (= (first a-seq) (first b-seq))
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false
;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [] [])             ;=> true
;(seq= [1 2 nil] [1 2])   ;=> false
;(seq= [1 4 2] [1 2 4])   ;=> false
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)) ))   )

;(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
  (cond
   (== 1 k) n
   (zero? k) 1
   :else (* n (power n (dec k)))) )

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (- n 2)) (fib (dec n)))))

;(fib 0) ;=> 0
;(fib 1) ;=> 1
;(fib 2) ;=> 1
;(fib 3) ;=> 2
;(fib 4) ;=> 3
;(fib 5) ;=> 5
;(fib 6) ;=> 8
;(fib 10) ;=> 55

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (> 1 how-many-times) '()
  :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
   ))

;(my-repeat 2 :a)    ;=> (:a :a)
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (<= up-to 0) '() (cons (dec up-to) (my-range (dec up-to)))))

;(my-range 0)  ;=> ()
;(my-range 1)  ;=> (0)
;(my-range 2)  ;=> (1 0)
;(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons (map (fn [x] x) a-seq) (tails (rest a-seq)))))

;(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq]
  (let [ b-seq (reverse a-seq)]
  (map reverse (tails b-seq))))

;(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn rotations [a-seq]
  ;(cond
   ;(empty? a-seq) '()
   ;()))
)

(defn my-frequencies-helper [freqs a-seq]
 ; (if (empty? a-seq)
  ;  freqs
   ; (if (contains))))
)

(defn my-frequencies [a-seq]
  ;(my-frequencies-helper a-seq))
)

;(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n)) '() (cons (first coll) (my-take (dec n)(rest coll))) ))

;(my-take 2 [1 2 3 4]) ;=> (1 2)
;(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (or (empty? coll) (> 1 n)) coll (my-drop (dec n) (rest coll)) ))

;(my-drop 2 [1 2 3 4]) ;=> (3 4)
;(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
 (vector (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (/ (count a-seq) 2) a-seq)))

;(halve [1 2 3])   ;=> [(1 2) (3 4)]
;(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;(halve [1])         ;=> [() (1)]

;(concat [1 2 3] [4])

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq) b-seq
    (if (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))
    ))

;(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)
;(seq-merge [1  3] [100 4])

(defn merge-sort [a-seq]
   (if (empty? a-seq)
     '()
     (if (singleton? a-seq)
       a-seq
       (let [[a b] (halve a-seq)]
         (seq-merge (merge-sort a)(merge-sort b))))))

;(merge-sort [])                 ;=> ()
;(merge-sort [1 2 3])            ;=> (1 2 3)
;(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])












