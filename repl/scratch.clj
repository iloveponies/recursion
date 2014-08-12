(use 'recursion :reload)

(longest-sequence [[8]])   ;=> [1 2]
(longest-sequence [#{1 2 5 9} [3 4 [7 9 6 5] 4] [5 8  8 5]]) ;=> [3 4]

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()
(my-filter even? [])

(sequence-contains? 3 [1 2 3 "pony"]) ;=> true
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
