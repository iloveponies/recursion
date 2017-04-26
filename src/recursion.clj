(ns recursion)

; This is linear reursion !!

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;; (product [])        ;=> 1  ; special case
;; (product [1 2 3])   ;=> 6
;; (product [1 2 3 4]) ;=> 24
;; (product [0 1 2])   ;=> 0
;; (product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

;; (singleton? [1])     ;=> true
;; (singleton? #{2})    ;=> true
;; (singleton? [])      ;=> false
;; (singleton? [1 2 3]) ;=> false

;;   (singleton? [1])     ;=> true
;;   (singleton? #{2})    ;=> true
;;   (singleton? [nil])   ;=> true
;;   (singleton? [1 nil]) ;=> false
;;   (singleton? [])      ;=> false
;;   (singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

;; (my-last [])      ;=> nil
;; (my-last [1 2 3]) ;=> 3
;; (my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

;; (max-element [2 4 1 4]) ;=> 4
;; (max-element [2])       ;=> 2
;; (max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

;; (seq-max [1] [1 2])   ;=> [1 2]
;; (seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

;; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;; (longest-sequence [[1 2]])            ;=> [1 2]
;; (longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

;; (my-filter odd? [1 2 3 4]) ;=> (1 3)
;; (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;; (my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

;; (sequence-contains? 3 [1 2 3]) ;=> true
;; (sequence-contains? 3 [4 7 9]) ;=> false
;; (sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

;; (my-take-while odd? [1 2 3 4])  ;=> (1)
;; (my-take-while odd? [1 3 4 5])  ;=> (1 3)
;; (my-take-while even? [1 3 4 5]) ;=> ()
;; (my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

;; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;; (my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    (and (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq))
    :else false))

;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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

(def fib
  (memoize
    (fn [n]
      (cond
        (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))))

;; (fib 0) ;=> 0
;; (fib 1) ;=> 1
;; (fib 2) ;=> 1
;; (fib 3) ;=> 2
;; (fib 4) ;=> 3
;; (fib 5) ;=> 5
;; (fib 6) ;=> 8
;; (fib 10) ;=> 55

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (<= how-many-times 0) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

;; (my-repeat 2 :a)    ;=> (:a :a)
;; (my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-repeat -1 :a)   ;=> ()

;; More correct, but not the answer!
;; (defn my-range [up-to]
;;   (defn inner[u]
;;     (if (>= u up-to)
;;       ()
;;       (cons u (inner (inc u)))))
;;   (inner 0))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

;; (my-range 0)  ;=> ()
;; (my-range 1)  ;=> (0)
;; (my-range 2)  ;=> (1 0)
;; (my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (apply list a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

;; (tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;; (inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
;; ; You can output the tails and inits in any order you like.
;; (inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(defn rotations [a-seq]
  (let [rot1 (fn [s]
               (concat (rest s) (list (first s))))
        rotn (fn rotn [s n]
          (if (<= n 0)
            ()
            (cons s (rotn (rot1 s) (dec n)))))
       ]
    (if (empty? a-seq)
      '(())
      (rotn (seq a-seq) (count a-seq)))))

;; (rotations [])        ;=> (())
;; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;; (rotations [:a :b])   ;=> ((:a :b) (:b :a))
;; ; The order of rotations does not matter.
;; (rotations [:a :b])   ;=> ((:b :a) (:a :b))
;; (rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
;; (count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (let [addone (fn[x]
                   (assoc freqs x (if (contains? freqs x)
                                    (inc (freqs x))
                                    1)))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (addone (first a-seq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (let [[k v] (first a-map)] (repeat v k)) (un-frequencies (rest a-map)))))

;; (un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (let [my-take-inner (fn my-take-inner [accum n coll]
          (if (or (<= n 0) (empty? coll))
            accum
            (my-take-inner (concat accum (list (first coll))) (dec n) (rest coll))))]
   (my-take-inner '() n coll)))

;; (my-take 2 [1 2 3 4]) ;=> (1 2)
;; (my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [split-seq (fn split-seq [accum a-seq n]
                    (if (or (<= n 0) (empty? a-seq))
                      [accum (seq a-seq)]
                      (split-seq (concat accum (list (first a-seq))) (rest a-seq) (dec n))))]
    (split-seq '() a-seq (quot (count a-seq) 2))))

;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (let [seq-merge-inner (fn seq-merge-inner [accum a-seq b-seq]
                          (cond
                            (and (empty? a-seq) (empty? b-seq)) (reverse accum)
                            (empty? a-seq) (seq-merge-inner (cons (first b-seq) accum) '() (rest b-seq))
                            (empty? b-seq) (seq-merge-inner (cons (first a-seq) accum) (rest a-seq) '())
                            (= (first a-seq) (first b-seq)) (seq-merge-inner (cons (first b-seq) (cons (first a-seq) accum)) (rest a-seq) (rest b-seq))
                            (< (first a-seq) (first b-seq)) (seq-merge-inner (cons (first a-seq) accum) (rest a-seq) b-seq)
                            :else                           (seq-merge-inner (cons (first b-seq) accum)  a-seq (rest b-seq))
                            ))]
  (seq-merge-inner '() a-seq b-seq)))

;; (seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;; (seq-merge [1 2 6 7] [9])        ;=> (1 2 4 6 7)
;; (seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (let [[s1 s2] (halve a-seq)]
    (if (or (<= (count s1) 1) (<= (count s2) 1)) ; if either s1 or s2 are 1 in length then the other value will be either 0 or 1 so that's the base case
      (seq-merge s1 s2)
      (seq-merge (merge-sort s1) (merge-sort s2)))))

;; (merge-sort [999 4 9 100 -2 2 3 1])
;; (merge-sort [])                 ;=> empty?
;; (merge-sort [1 2 3])            ;=> '(1 2 3)
;; (merge-sort [5 3 4 17 2 100 1]) ;=> '(1 2 3 4 5 17 100))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])



