;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(use 'recursion :reload)



(my-take 3 '(1 2 3 4))
(my-take 6 '(1 2))

(cons 1 [2 3])

(my-drop 3 '(1 2 3 4))

(halve '(1 2 3 4 5))

(first '())

(< 1 nil)

(seq-merge '(1 2) '(3 4))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(halve '())

(merge-sort '(5 8 2 4 9))



(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))

(monotonic? '())

(monotonic-until [5 6 1])

(inits [1 2 1 7 2 5])

(monotonic-until '((1)) 1)


(defn monu [inits-seq & [n]]
  (let [counter (or n 0)]
  (cond
   (empty? inits-seq) 0
   (monotonic? (first inits-seq)) (monu (rest inits-seq) (inc counter))
   :else counter)))

(monu (rest (inits [1 4 5 6 1 2 3])) 0)

(inits [1 4 1])
(rest (inits[1 4 1]))

(split-into-monotonics [1 2 3 2 1])

(rest (inits '()))

(defn split-into-monotonic [a-seq]
  (let [inits-seq (rest (inits a-seq))]
    (cond
     (empty? a-seq) '()
     :else (cons
            (take (monotonic-until inits-seq) a-seq)
            (split-into-monotonic (drop (monotonic-until inits-seq) a-seq))))))

(split-into-monotonic [1 2 1 2 3 7 6 4])

(monotonic-until (rest (inits [2 1 0 1])))
