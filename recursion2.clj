;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(let [[x y] []]
  x)

(= [nil] [])
(get [nil] 1)

(empty? [nil])
(get [1 2 3] 0)

[5 6]

(use 'recursion)

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false

(max 0 0)

(first '())
(= '() '())

(cons '() '())

(rest [1])

(= [1 :a 2] [1 :a 2])

(rest nil)

(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))

; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(def x {nil 4 :a 5 :b 6})

(assoc x nil 5)
(get x :c)

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1])
;=> {:a 3, "moi" 3, 1 1}

(into [] x)
(map (fn [[x y]] (repeat y x)) x)


(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)


(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()


(defn first-mono-helper2 [opt mono-seq bb]
  (if (opt (first mono-seq) (first part-seq))
    (first-mono-helper2 opt
                       (cons (first part-seq) mono-seq)
                       (rest part-req))
    ([mono-seq part-seq])))





