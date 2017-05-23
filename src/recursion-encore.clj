(use 'recursion)
(use 'midje.repl)
(require 'recursion :reload)
(use 'spyscope.repl)

;; split-into-monotonics, explained
;; assume input '(0 1 1 2 1 0 1)

(def a-seq '(0 1 1 2 1 0 1))

(inits a-seq)
(into () (inits a-seq)) ;; clojure way of reordering
(rest (into () (inits a-seq))) ;; filter empty in constant time

;; collect all strictly monotonic seqs

(def monotonic-coll (take-while #(or (apply < %) (apply > %))
                                (rest (into () (inits a-seq)))))

;; take only the longest sequence available (in linear time)

(last monotonic-coll)

;; drop coll from a-seq

(drop (count monotonic-coll) a-seq)

;; the above form recurs until (empty? a-seq)
;; recursion exhaustively

(cons '(0 1)
      (cons '(1 2)
            (cons '(1 0)
                  (cons '(1) nil ;; base case returns a-seq, which is nil
                        ))))






;; permutations, explained
;; assume input '(:a :b :c)

(def a (map #(cons :a %) '((:b :c) (:c :b))))
(def b (map #(cons :b %) '((:a :c) (:c :a))))
(def c (map #(cons :c %) '((:a :b) (:b :a))))

(fact (apply concat (list a b c)) => '((:a :b :c) (:a :c :b)
                                       (:b :a :c) (:b :c :a)
                                       (:c :a :b) (:c :b :a)))






;; powerset, explained
;; assume input #{:a :b :c}

(def a (set (map set (map #(cons :a %) #{#{:b :c} #{:b} #{:c}}))))
(def b (set (map set (map #(cons :b %) #{#{:a :c} #{:a} #{:c}}))))
(def c (set (map set (map #(cons :c %) #{#{:a :b} #{:a} #{:b}}))))

(def redundant-powerset (apply concat (list [#{}]
                                      [#{:a}]
                                      [#{:b}]
                                      [#{:c}]
                                      ;; colls
                                      a b c)))

(fact (set redundant-powerset) => #{#{}
                                    #{:a}
                                    #{:b}
                                    #{:c}
                                    #{:a :c}
                                    #{:a :b}
                                    #{:c :b}
                                    #{:a :c :b}})
