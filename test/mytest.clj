(ns mytest)

(use 'recursion)

(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false
(singleton? [nil])   ;=> true

(or (first [nil]) true)

(nil? (first [nil 1]))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(max 2 23)

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil
(max-element [10 20 30 40])        ;=> nil

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]
(seq-max [1 2 ] nil)  ;=> [1 2]

(vec (rest [[1 2] [] [1 2 3]]))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])                 ;=> nil

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8
(fib 10) ;=> 55

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(tails [1 2 3 4 5]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(my-frequencies []) ;=> {}
(my-frequencies [:c :a :b :a :b :b :a :c]) ;=> {:a 3, "moi" 3, 1 1}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


(un-frequencies {})
(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 15})) ;=> {:a 100 :b 10}

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(let [[x y] (halve [1 5 7 9 2 2 8 10 11 12])]
  (concat x y)
  )


(merge-sort [])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(is-monotonic? [0 2 3 5 1])

(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))

;(permutations #{}) ;=> (())
;(permutations #{1 5 3}) ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))
(permutations-helper #{1 2 3 4}) ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))

(rotations #{1 2 3 4})
