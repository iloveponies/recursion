(ns recursion)



;; (defn product
;;   ([] 1)
;;   ([coll] (let [[x & a-set] coll]
;;             (* x (product  a-set)))))


(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

;; (product [])        ;=> 1  ; special c
;; (product [1 2 3])   ;=> 6
;; (product [1 2 3 4]) ;=> 24
;; (product [0 1 2])   ;=> 0
;; (product #{2 3 4})  ;=> 24 ; works for sets too!


(defn singleton? [coll]
  (and
    ((complement empty?) coll)
    (empty? (rest coll))))

;; (singleton? [1])     ;=> true
;; (singleton? #{2})    ;=> true
;; (singleton? [])      ;=> false
;; (singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (first (reverse coll)))

;; (my-last [])      ;=> nil
;; (my-last [1 2 3]) ;=> 3
;; (my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    :else (apply max a-seq)))

;; (max-element [2 4 1 4]) ;=> 4
;; (max-element [2])       ;=> 2
;; (max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (let [ len1 (count seq-1)
         len2 (count seq-2)]
    (if (<= len1 len2)
      seq-2
      seq-1)))

;; (seq-max [1] [1 2])   ;=> [1 2]
;; (seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    :else (reduce (fn [acc x]
           (seq-max acc x))
         ()
         a-seq)))

;; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;; (longest-sequence [[1 2]])            ;=> [1 2]
;;  (longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (let [[x & new-seq] a-seq]
    (cond
      (empty? a-seq) ()
      (pred? x) (cons x (my-filter pred? new-seq))
      :else (my-filter pred? new-seq))))

;;   (my-filter odd? [1 2 3 4]) ;=> (1 3)
;;   (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;;   (my-filter even? [1 3 5 7]) ;=> ()


(defn sequence-contains? [elem a-seq]
  (let [ elem=? #(= elem %)]
  (cond
      (empty? a-seq) false
      (elem=? (first a-seq)) true
      :else (sequence-contains? elem (rest a-seq)))))

;;  (sequence-contains? 3 [1 2 3]) ;=> true
;;  (sequence-contains? 4 [1 2 3]) ;=> false
;;  (sequence-contains? :pony [])  ;=> false


(defn my-take-while [pred? a-seq]
  (let [ [ x & new-seq] a-seq]
 (cond
    (empty? a-seq) ()
    (pred? x) (cons x (my-take-while pred? new-seq))
    :else ()))
  )

;; (my-take-while odd? [1 2 3 4])  ;=> (1)
;; (my-take-while odd? [1 3 4 5])  ;=> (1 3)
;; (my-take-while even? [1 3 4 5]) ;=> ()
;; (my-take-while odd? [])  ;=> ()


(defn my-drop-while [pred? a-seq]
  (let [ [ x & new-seq] a-seq]
 (cond
    (empty? a-seq) ()
    (pred? x) (my-drop-while pred? new-seq)
    :else a-seq)))

;; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;; (my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
 (let [len-a (count a-seq)
       len-b (count b-seq)]
   (cond
     ((complement =) len-a len-b)
       false
     (and  (empty? a-seq)(empty? b-seq))
       true
     (= (first a-seq) (first b-seq))
       (seq= (rest a-seq)(rest b-seq))
     :else false)))

;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false


(defn my-map [f seq-1 seq-2]
 (let [ f-seq #(+ (f %1) %2)]
 (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f-seq (first seq-1) (first seq-2))
               (my-map f (rest seq-1)
                         (rest seq-2))))))

;; (my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;; (my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;; (my-map + [1 2 3] [])        ;=> ()


(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

;; (power 2 2)  ;=> 4
;; (power 5 3)  ;=> 125
;; (power 7 0)  ;=> 1
;; (power 0 10) ;=> 0

(defn list-fib
  ([] (cons 0 (cons 1 (list-fib 0 1))))
  ([n p] (let [ n+ (+ n p)]
          (cons n+ (lazy-seq (list-fib p n+))))))

;;(take 11 (list-fib)) ;=>(0 1 1 2 3 5 8 13 21 34 55)


(defn fib [n]
  (last (take (+ 1 n) (list-fib))))

;; (fib 0) ;=> 0
;; (fib 1) ;=> 1
;; (fib 2) ;=> 1
;; (fib 3) ;=> 2
;; (fib 4) ;=> 3
;; (fib 5) ;=> 5
;; (fib 6) ;=> 8
;; (fib 10) ;=> 55


(defn  helper [n n- i j]
  (cond
    (= i j) n
    :else (helper  (+ n n-) n (inc i) j)))

(defn list-fib1 [j]
  "fibonaci without lazy-seq"
  (helper 1 0 0 j))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times ) ()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


;; (my-repeat 2 :a)    ;=> (:a :a)
;; (my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-repeat -1 :a)   ;=> ()

(defn lazy-repeat
  ([what-to-repeat]  (cons what-to-repeat (lazy-seq (lazy-repeat what-to-repeat)))))

;;(take 10 (lazy-repeat "*"));=>("*" "*" "*" "*" "*" "*" "*" "*" "*" "*")

(defn my-lazy-repeat [ how-many-times what-to-repeat]
  (take  how-many-times  (lazy-repeat what-to-repeat)))

;; (my-lazy-repeat 2 5 :a)    ;=> (:a :a)
;; (my-lazy-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-lazy-repeat -1 :a)   ;=> ()

(defn my-lazy-range
  ([] (my-lazy-range 0))
  ([n] (cons  n (lazy-seq (my-lazy-range (inc n))) )))

(defn my-range [up-to]
   (reverse (take up-to (my-lazy-range))))

;; (my-range 0)  ;=> ()
;; (my-range 1)  ;=> (0)
;; (my-range 2)  ;=> (1 0)
;; (my-range 3)  ;=> (2 1 0)


(defn tails [ a-seq]
  (let [ a-seq (apply list a-seq)]
  (cond
    (empty? a-seq)  (cons () ())
    :else (cons  a-seq (tails (rest a-seq))))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


;;(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;;(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
;;(inits [1 2 3  4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))



(defn liste-rotation [rot-seq a-seq]
  (cond
    (empty? a-seq) ()
    (= a-seq rot-seq) ()
    :else (cons rot-seq (liste-rotation (cons (last rot-seq)
                                (reverse (rest (reverse rot-seq))))
                          a-seq ))))

(defn rotations [a-seq]
  (let [ a-seq (apply list a-seq)
         start  (cons (last a-seq)(reverse (rest (reverse a-seq))))]
   (cons a-seq (liste-rotation start a-seq))))


;; (rotations [])        ;=> (())
;; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;; (rotations [:a :b])   ;=> ((:a :b) (:b :a))
;; (rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;; (count (rotations [6 5 8 9 2])) ;=> 5


(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (let [ elt (first a-seq)
                 counter (get freqs elt 0)
                 new-freqs (assoc freqs elt (inc counter))]
            (my-frequencies-helper new-freqs (rest a-seq)))))

;;(my-frequencies-helper {} [ 1 2 1 1 1 1])=>{2 1, 1 5}

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


(defn key+ [ a-set elt]
  (assoc a-set elt (inc (get a-set elt 0))))

(defn new-frequencies [a-seq]
  (reduce (fn [freqs elt]
            (key+ freqs elt))
          {}
          a-seq))
;;(new-frequencies [1 2 1 1])

(defn un-frequencies [ a-map ]
  (cond
    (empty? a-map) ()
    :else  (let [ [ elt counter ] (first a-map)
                  list-repeat (my-repeat counter elt)]
               (concat  list-repeat  (un-frequencies (rest a-map))))))

;; (un-frequencies {:a 3 :b 2 "^_^" 1});=> (:a :a :a "^_^" :b :b)
;; (un-frequencies {:a 3 });=> (:a :a :a )
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100  :b 100 })) ;=> {:a 100 :b 10}


(defn my-take [n coll]
  (let [min-count (min n (count coll))]
  (cond
   (= min-count  0) ()
    :else (cons (first coll)
                (my-take (dec n)(rest coll))))))

;;  (my-take 2 [1 2 3 4]) ;=> (1 2)
;;  (my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (cond
    (>= n (count coll)) ()
    :else (reverse (my-take (- (count coll) n) (reverse coll)))))

;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [ middle (int (/ (count a-seq) 2))]
    (cons (take middle a-seq) (cons (drop middle a-seq) ()))))

;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (let [ [first-a  & rest-a] a-seq
         [first-b  & rest-b] b-seq]
  (cond
    (empty? a-seq)
      (cond
        (empty? b-seq) ()
        :else  b-seq)
    (empty? b-seq)
      (cond
        (empty? a-seq) ()
        :else  a-seq)
    (<= first-a first-b) (cons first-a (seq-merge rest-a b-seq))
    :else (cons first-b (seq-merge a-seq rest-b)))))

;; (= (seq-merge [1 6 9] [0 2 3 4 5 7 8 ])  (range 10))
;; (seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;; (seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
 (let [ [ halve-l halve-r] (halve a-seq)
        ]
   (cond
     (empty? a-seq) ()
     (= 1 (count a-seq)) a-seq
     :else (seq-merge (merge-sort halve-l) (merge-sort halve-r)))))


;; (merge-sort [])                 ;=> ()
;; (merge-sort [1 2 3])            ;=> (1 2 3)
;; (merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)


(defn my-sorted? [liste]
  (let [list-iter (map vector liste (rest liste))]
  (reduce (fn [acc pair]
            (and (<= (first pair)  (second pair))
                 acc))
          true
          list-iter)))

;; (sorted? [1 2 3]) ;=> true
;; (sorted? [1 3 2]) ;=> false


(defn prefix-valid-max [listes]
(last
    (take-while  #(my-sorted? %) listes)))


;; (prefix-valid-max [[ 0 ] [ 0  1] [ 0 1 2]]) ;=>[0 1 2]
;; (prefix-valid-max [[ 0 ] [ 0 3 1] [ 0 1 2]]);=>[0]

(defn prefix-valid-max-reverse [listes]
  (last
    (take-while  #(my-sorted? (reverse %)) listes)))

;;(prefix-valid-max-reverse (inits [ 2 1 0]));=>(2 1 0)

(defn prefix [liste]
  (let [ p (count (prefix-valid-max liste))
         l (count (prefix-valid-max-reverse liste))]
  (cond
    (< l p) (prefix-valid-max liste)
    (< p l) (prefix-valid-max-reverse liste)
    :else ())))

;(prefix [[0 1] [0 1 2]]);=>[0 1 2]
;(prefix [[2 1] [2 1 0]]);=>[2 1 0]
;(prefix [[]]);=>()

;(prefix ['() '(1) '(1 0)])

(defn not-singleton? [liste]
  (< 1 (count liste)))

;; (not-singleton? []) ;=>false
;; (not-singleton? [1]);=>false
;; (not-singleton? [1 2]);=>true


(defn split-into-monotonics [a-seq]
 (let [ list-prefix (inits a-seq)
        prefix-max (prefix list-prefix)
        new-list  (drop (count prefix-max) a-seq)
        ]
   (cond
   (empty? new-list) (cons prefix-max ())
   :else  (cons  prefix-max  (split-into-monotonics new-list)))))


;; (split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
;; (split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))


(defn add-elt [liste x]
  (map #(conj %  x) liste))

;;(= (add-elt [[1 2 3] [1 5 6] ]4) [[1 2 3 4 ] [1 5 6 4] ])

(defn rotation-liste [listes]
  (apply concat (map #(rotations %) listes)))

;;(rotation-liste [[1 2] [2 3]])


(defn helper-permut [a-seq]
  (cond
    (singleton? a-seq)  [ [a-seq] ]
    :else (let [ [ x & arg ] a-seq]
                  (for [ solution (helper-permut (into [] arg))]
                      (apply concat (map #(rotations %) (add-elt solution x)))))))


(defn permutations [a-set]
  (cond
    (empty? a-set) (list '())
    :else (apply concat (helper-permut (into '() a-set)))))




(defn powerset [a-set ]
 (set
   (map set (apply concat
                      (map #(inits % ) (permutations a-set))))))



;;  (powerset #{})      ;=> #{#{}}
;;  (powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}



