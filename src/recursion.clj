(ns recursion)

;; (product [])        ;=> 1  ; special case
;; (product [1 2 3])   ;=> 6
;; (product [1 2 3 4]) ;=> 24
;; (product [0 1 2])   ;=> 0
;; (product #{2 3 4})  ;=> 24 ; works for sets too!
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

;; (singleton? [1])     ;=> true
;; (singleton? #{2})    ;=> true
;; (singleton? [])      ;=> false
;; (singleton? [1 2 3]) ;=> false
(defn singleton? [coll]
  (and (not= [] coll) (empty? (rest coll))))

;; (my-last [])      ;=> nil
;; (my-last [1 2 3]) ;=> 3
;; (my-last [2 5])   ;=> 5
(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))
    )
  )

;; (max-element [2 4 1 4]) ;=> 4
;; (max-element [2])       ;=> 2
;; (max-element [])        ;=> nil
(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

;; (seq-max [1] [1 2])   ;=> [1 2]
;; (seq-max [1 2] [3 4]) ;=> [3 4]
(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

;; (longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
;; (longest-sequence [[1 2]])            ;=> [1 2]
;; (longest-sequence [])                 ;=> nil
(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence(rest a-seq)))))

;; (my-filter odd? [1 2 3 4]) ;=> (1 3)
;; (my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
;; (my-filter even? [1 3 5 7]) ;=> ()
(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

;; (sequence-contains? 3 [1 2 3]) ;=> true
;; (sequence-contains? 3 [4 7 9]) ;=> false
;; (sequence-contains? :pony [])  ;=> false
(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

;; (sequence-contains? 3 [1 2 3]) ;=> true
;; (sequence-contains? 3 [4 7 9]) ;=> false
;; (sequence-contains? :pony [])  ;=> false
(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

;; (my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
;; (my-drop-while odd? [1 3 4 5])  ;=> (4 5)
;; (my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
;; (my-drop-while odd? [])         ;=> ()
(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

;; (seq= [1 2 4] '(1 2 4))  ;=> true
;; (seq= [1 2 3] [1 2 3 4]) ;=> false
;; (seq= [1 3 5] [])        ;=> false
(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (count a-seq) (count b-seq)) false
   :else (if (= (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq) )
           false)))

;; (my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
;; (my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
;; (my-map + [1 2 3] [])        ;=> ()
(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

;; (power 2 2)  ;=> 4
;; (power 5 3)  ;=> 125
;; (power 7 0)  ;=> 1
;; (power 0 10) ;=> 0
(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

;; (fib 0) ;=> 0
;; (fib 1) ;=> 1
;; (fib 2) ;=> 1
;; (fib 3) ;=> 2
;; (fib 4) ;=> 3
;; (fib 5) ;=> 5
;; (fib 6) ;=> 8
;; ...
;; (fib 10) ;=> 55
(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

;; (my-repeat 2 :a)    ;=> (:a :a)
;; (my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
;; (my-repeat -1 :a)   ;=> ()
(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

;; (my-range 0)  ;=> ()
;; (my-range 1)  ;=> (0)
;; (my-range 2)  ;=> (1 0)
;; (my-range 3)  ;=> (2 1 0)
(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

;; The order of elements in the output sequence doesn’t matter.
; (tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

;; The order of elements in the output sequence doesn’t matter.
; (inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (map reverse (reverse (tails (reverse a-seq))))
    )
  )

;; (rotations [])        ;=> (())
;; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
;; (rotations [:a :b])   ;=> ((:a :b) (:b :a))
;; ; The order of rotations does not matter.
;; (rotations [:a :b])   ;=> ((:b :a) (:a :b))
;; (rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(defn rotations [a-seq]
  (def helper (fn [rots f-seq]
    (if (< rots 1)
      '()
      (cons (seq f-seq) (helper (dec rots) (cons (last f-seq) (drop-last f-seq)))))))
  (if (empty? a-seq)
    '(())
    (helper (count a-seq) a-seq)))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          new-freqs (assoc freqs head (inc (get freqs head 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

;; (my-frequencies []) ;=> {}
;; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;; The order of elements in the output sequence doesn’t matter.
;; (un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
;; (un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
;; (my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}
(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (my-repeat v k) (un-frequencies (rest a-map)))))
  )

;; (my-take 2 [1 2 3 4]) ;=> (1 2)
;; (my-take 4 [:a :b])   ;=> (:a :b)
(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n)(rest coll)))))

;; (my-drop 2 [1 2 3 4]) ;=> (3 4)
;; (my-drop 4 [:a :b])   ;=> ()
(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

;; (halve [1 2 3 4])   ;=> [(1 2) (3 4)]
;; (halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
;; (halve [1])         ;=> [() (1)]
(defn halve [a-seq]
  (let [lhs (int (/ (count a-seq) 2))]
    [(my-take lhs a-seq) (my-drop lhs a-seq)]))

;; (seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
;; (seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)
(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else
   (let [a (first a-seq)
         b (first b-seq)]
     (if (< a b)
       (cons a (seq-merge (rest a-seq) b-seq))
       (cons b (seq-merge a-seq (rest b-seq)))))))

;   (merge-sort [4 2 3 1])
;=> (seq-merge (merge-sort (4 2))
;              (merge-sort (3 1)))
;=> (seq-merge (seq-merge (merge-sort (4))
;                         (merge-sort (2)))
;              (seq-merge (merge-sort (3))
;                         (merge-sort (1))))
;=> (seq-merge (seq-merge (4) (2))
;              (seq-merge (3) (1)))
;=> (seq-merge (2 4) (1 3))
;=> (1 2 3 4)
(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[lhs rhs] (halve a-seq)]
      (seq-merge (merge-sort lhs) (merge-sort rhs))
      )
    ))

;; (split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
;; (split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))
(defn monotonic?[a-seq]
  (or (empty? a-seq)
      (singleton? a-seq)
      (apply < a-seq)
      (apply > a-seq)))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [monotone (last (take-while monotonic? (inits a-seq)))
          the-rest (my-drop (count monotone) a-seq)]
      (cons monotone (split-into-monotonics the-rest)))))

;; (permutations #{}) ;=> (())
;; (permutations #{1 5 3}) ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))
(defn p-helper[idx a-set]
  (if (< idx 1)
    '()
    (concat
     (map (fn[e] (cons (first a-set) e)) (rotations (rest a-set)))
     (p-helper (dec idx) (cons (last a-set) (drop-last a-set))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (p-helper (count a-set) a-set)))

(defn powerset [a-set]
  [:-])
