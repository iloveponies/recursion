;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(use 'recursion :reload)

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
(my-last [2 5])  ;=> 5

(max-element [2 7 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> ni

(max 4 0)

(seq-max [1] [1 2]) ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])                 ;=> nil

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()

(sequence-contains? 3 [1 2 3]);=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])      ;=> ()

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])      ;=> false

(first [])
(= nil 1)

(defn seqi [a-seq b-seq]
  (cond
   (empty? a-seq) (if (empty? b-seq)
                    true
                    false)
   (empty? b-seq) (if (empty? a-seq)
                   true
                   false)
   :else          (if (= (first a-seq) (first b-seq))
                    (seqi (rest a-seq) (rest b-seq))
                    false)))

(seqi [] [0])
(empty? [])

(my-map + [1 2 3] [4 4 4])  ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])     ;=> ()


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

(my-repeat 3 "loll")    ;=> (:a :a)
(my-repeat 3 "loli") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(tails [1 2 3 4])


(rest [1])

(cons '(1) '(()))

(inits [1 2 3 4]);=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(cons '() '(1))

(rotations [])  ;=> (())
(rotations [1 2 3]) ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn rotar [head tail]
  (cond
   (empty? tail) '()
   :else (cons
          (concat (rest tail) (conj head (first tail)))
          (rotar (conj head (first tail)) (rest tail)))))

(rotar '() '(1 2))

(defn rota [a-seq]
  (let
    [rotator (fn rotator [head tail]
                  (cond
                   (empty? tail) '()
                   :else (cons
                          (concat (rest tail) (conj head (first tail)))
                          (rotator (conj head (first tail)) (rest tail)))))]
    (cond
     (empty? a-seq) '(())
     :else (rotator '() a-seq))))

(rota [1 2])

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(un-frequencies {:a 3 :b 2 "^_^" 1} )          ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}


(rest (first {:a 1 :b 2}))

(defn un-freq [a-map]
  (cond
   (empty? a-map) a-map
   :else   (let [current-key (key (first a-map))
                 current-value (val (first a-map))]
             (concat
              (repeat current-value current-key)
              (un-freq (rest a-map)) ))))

(un-freq {:a 3 :b 2 "^_^" 1} )
