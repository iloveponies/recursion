(use 'recursion :reload)

(defn first-in [val seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) 0
    (= (first seq-1) val) 1
    (= (first seq-2) val) 2
    :else (first-in val (rest seq-1) (rest seq-2))))

(first-in 3 [2 3 4] [4 6 3])

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 nil] [1 2 nil 5]) ;=> false
(seq= [1 3 5] [])        ;=> false
(seq= [1] '(1 nil))

(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(vector 1 2)     ;=> [1 2]
(vector 1 2 3 4) ;=> [1 2 3 4]

(map vector [1 2 3] [:a :b :c])

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(indexed [:a :b :c]) ;=> ([0 :a] [1 :b] [2 :c])

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(consecutives [:a :b :c]) ;=> ([:a :b] [:b :c])

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(fib 10)

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn inits2 [coll]
  "create sequence of all initial subsets"
  (reverse (map reverse (tails (reverse coll)))))
(cons '(:a) (seq '(())))

(defn inits3 [coll]
  (if (empty? coll)
    '(())
    (cons (seq coll) (lazy-seq (tails (next coll))))))

(tails '(:x 23 56 1))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))
