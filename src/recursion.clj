(ns recursion)


; EXERCISE 1
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;(product [])        ;=> 1  ; special case
;(product [1 2 3])   ;=> 6
;(product [1 2 3 4]) ;=> 24
;(product [0 1 2])   ;=> 0
;(product #{2 3 4})  ;=> 24 ; works for sets too!


; EXERCISE 2
; Write down the evaluation of (product [1 2 4]) like we did for sum above.
; This exercise does not give any points and you do not need to return it.


; EXERCISE 3
(defn singleton? [coll]
  (cond
     (empty? coll) false
     (empty? (rest coll)) true
     :else false))

;(singleton? [1])     ;=> true
;(singleton? #{2})    ;=> true
;(singleton? [])      ;=> false
;(singleton? [1 2 3]) ;=> false


; EXERCISE 4
(defn single-or-empty [a-seq]
  (or (singleton? a-seq) (empty? a-seq)))

(defn my-last [a-seq]
  (if (single-or-empty a-seq)
    (first a-seq)
    (my-last (rest a-seq))))

;(my-last [])      ;=> nil
;(my-last [1 2 3]) ;=> 3
;(my-last [2 5])   ;=> 5


; EXERCISE 5
(defn max-element [a-seq]
  (if (single-or-empty a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

;(max-element [2 4 1 4]) ;=> 4
;(max-element [2])       ;=> 2
;(max-element [])        ;=> nil


; EXERCISE 6
(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

;(seq-max [1] [1 2])   ;=> [1 2]
;(seq-max [1 2] [3 4]) ;=> [3 4]


; EXERCISE 7
(defn longest-sequence [a-seq]
  (if (single-or-empty a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

;(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;(longest-sequence [[1 2]])            ;=> [1 2]
;(longest-sequence [])                 ;=> nil


; EXERCISE 8
(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else  (my-filter pred? (rest a-seq))))

;(my-filter odd? [1 2 3 4]) ;=> (1 3)
;(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;(my-filter even? [1 3 5 7]) ;=> ()


; EXERCISE 9
(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

;(sequence-contains? 3 [1 2 3]) ;=> true
;(sequence-contains? 3 [4 7 9]) ;=> false
;(sequence-contains? :pony [])  ;=> false


; EXERCISE 10
(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond (empty? a-seq) ()
          (pred? f) (cons f (my-take-while pred? r))
          :else '() )))

;(my-take-while odd? [1 2 3 4])  ;=> (1)
;(my-take-while odd? [1 3 4 5])  ;=> (1 3)
;(my-take-while even? [1 3 4 5]) ;=> ()
;(my-take-while odd? [])         ;=> ()


; EXERCISE 11
(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

;(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;(my-drop-while odd? [])         ;=> ()


; EXERCISE 12
(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false


; EXERCISE 13
(defn mytool [f a b]
  (seq [(f (first a) (first b) )]))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) []
         (or (singleton? seq-1) (singleton? seq-2)) (mytool f seq-1 seq-2)
         :else (cons (f (first seq-1) (first seq-2))
                     (my-map f (rest seq-1) (rest seq-2)))))

;(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;(my-map + [1 2 3] [])        ;=> ()


; EXERCISE 14
(defn power [n k]
  (cond (= 0 k) 1
        (= 1 k) n
        (< k 0) (/ (power n (- k)))
        :else (* n (power n (- k 1)))))

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0


; EXERCISE 15
(defn fib [n]
  (cond (= 0 n) 0
        (<= n 2) 1
        :else (+ (fib (- n 2)) (fib (- n 1)))))

;(fib 0) ;=> 0
;(fib 1) ;=> 1
;(fib 2) ;=> 1
;(fib 3) ;=> 2
;(fib 4) ;=> 3
;(fib 5) ;=> 5
;(fib 6) ;=> 8
;(fib 10) ;=> 55


; EXERCISE 16
(defn my-repeat [n x]
  (cond  (<= n 0) []
          (= n 1) [x]
          :else (cons x (my-repeat (- n 1) x))))

;(my-repeat 2 :a)    ;=> (:a :a)
;(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;(my-repeat -1 :a)   ;=> ()


; EXERCISE 17
(defn my-range [n]
  (let [x (- n 1)]
   (cond  (< n 1) []
          (= n 1) [x]
          :else (cons x (my-range x)))))

;(my-range 0)  ;=> ()
;(my-range 1)  ;=> (0)
;(my-range 2)  ;=> (1 0)
;(my-range 3)  ;=> (2 1 0)


; EXERCISE 18
(defn tails [a-seq]
  (defn tails [a-seq]
  (let [ s (seq a-seq) ]
    (cond (empty? a-seq) '([])
          :else (cons s (tails (rest a-seq)))) )))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

;(reverse [1 2 3]) ;=> (3 2 1)
;(reverse [2 3 1]) ;=> (1 3 2)


; EXERCISE 19
(defn rotations [a-seq]
  (if (empty? a-seq) '([])
    (let [i (inits a-seq)
          t (reverse (tails a-seq))
          fi (first i)
          ft (first t)]
      (rest (my-map concat t i)))))

;(rotations [])        ;=> (())
;(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
;(rotations [:a :b])   ;=> ((:b :a) (:a :b))
;(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;(count (rotations [6 5 8 9 2])) ;=> 5


; EXERCISE 20
(defn my-frequencies-helper [freqs a-seq]
  (if (or (nil? a-seq) (empty? a-seq)) freqs
  (let [w (first a-seq)
        r (rest  a-seq)
        c (if (contains? freqs w) (inc (get freqs w)) 1)
        nfreqs (assoc freqs w c)]
    (my-frequencies-helper nfreqs r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;(my-frequencies []) ;=> {}
;(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


; EXERCISE 21
(defn un-frequencies [a-map]
  (let [x a-map
        y (first x)
        z (rest x)
        f (if (or (nil? x) (empty? x)) [] (my-repeat (val y) (key y)) )]
    (cond (or (nil? x) (empty? x)) []
          (singleton? x) f
          :else (concat f (un-frequencies z)))
    ; f
  ))

;(first {:a 1 :b 2}) ;=> [:a 1]
;(rest {:a 1 :b 2 :c 3}) ;=> ([:b 2] [:c 3])


; EXERCISE 22
(defn my-take [n coll]
  (let [x (first coll)
        r (rest coll)]
    (cond  (<= n 0) []
           (or (empty? coll) (nil? coll)) []
           (= n 1) [x]
           :else (cons x (my-take (- n 1) r)))))

;(my-take 2 [1 2 3 4]) ;=> (1 2)
;(my-take 4 [:a :b])   ;=> (:a :b)


; EXERCISE 23
(defn my-drop [n coll]
  (let [x (first coll)
        r (rest coll)]
    (cond  (or (empty? coll) (nil? coll)) []
           (= n 0) coll
           :else (my-drop (- n 1) r))))

;(my-drop 2 [1 2 3 4]) ;=> (3 4)
;(my-drop 4 [:a :b])   ;=> ()


; EXERCISE 24
(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

;(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;(halve [1])         ;=> [() (1)]


; EXERCISE 25
(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        ar (rest a-seq)
        br (rest b-seq)]
    (cond (or (empty? a-seq) (nil? a-seq)) b-seq
          (or (empty? b-seq) (nil? b-seq)) a-seq
          (< a b) (cons a (seq-merge ar b-seq))
          :else (cons b (seq-merge a-seq br)))))

;(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)


; EXERCISE 26
(defn merge-sort [a-seq]
  (if (or (nil? a-seq) (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

;(merge-sort [])                 ;=> ()
;(merge-sort [1 2 3])            ;=> (1 2 3)
;(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)


; EXERCISE 27 2 points
(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) ()
    (let [init (rest (reverse (inits a-seq)))
          a    (last (my-take-while (fn [x] (= (last x) (max-element x))) init))
          b    (last (my-take-while (fn [x] (= (first x) (max-element x))) init))
          c    (seq-max a b)
          n    (count c)
          r    (my-drop n a-seq)]
      (concat [c] (split-into-monotonics r))
      )))

;(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
;(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))


; EXERCISE 28 3 points
(defn permutations [a-set]
  (cond
   (empty? a-set) [()]
   (vector? a-set)
   (if (= 1 (count a-set))
     (list [(a-set 0)])
     (loop [i 0
            permutaatiot '()]
       (if (= i (count a-set))
         permutaatiot
         (let [muut (into [] (concat (my-take i a-set) (my-drop (inc i) a-set)))
               muu-muut (permutations muut)
               uudet-muut (map #(conj % (a-set i)) muu-muut)]
           (recur (inc i) (into permutaatiot uudet-muut))))))
   :else (permutations (into [] a-set))))

;(permutations #{}) ;=> (())
;(permutations #{1 5 3}) ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))


; EXERCISE 29 3 points
(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [ps (powerset (rest a-set))]
    (concat ps
            (map #(conj % (first a-set)) ps)))))

;(powerset #{})      ;=> #{#{}}
;(powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}

