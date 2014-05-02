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

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil
