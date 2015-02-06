(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (= 1 (count coll)))

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (cond
    (empty? coll)
      nil
    (singleton? coll)
      (first coll)
    :else
      (my-last (rest coll))))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
     (first a-seq)
    :else
      (max (first a-seq) (max-element (rest a-seq)))))

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(empty? [])

(seq-max [1] nil)   ;=> [1 2]
(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
     (seq-max (first a-seq) (longest-sequence (rest a-seq)))
   ))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])

(defn my-filter [pred? [fst & rst :as all]]
  (if (empty? all)
    '()
    (let [filtered-rest (my-filter pred? rst)]
      (if (pred? fst)
        (cons fst filtered-rest)
        filtered-rest))))

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? (range 1 50)) ;=> ()

  (my-filter odd? [1 2 3 4])
  (my-filter false? [1 2 3])
  (my-filter nil? [1 nil 2])
  (my-filter (fn [x] (> x 9000)) [12 49 90 9001])
  (my-filter even? [1 3 5 7])

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? [fst & rst]]
  (cond
    (nil? fst)
      '()
    (pred? fst)
      (cons fst (my-take-while pred? rst))
    :else
      '()))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? [fst & rst :as all]]
  (cond
    (nil? fst)
      '()
    (pred? fst)
      (my-drop-while pred? rst)
    :else
      all))

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

  (seq= [1 2 4] '(1 2 4))
  (seq= [] [])
  (seq= [1 2 nil] [1 2])
  (seq= [1 4 2] [1 2 4])
  (seq= [1 2 3] [1 2 3 4])
  (seq= [1 3 5] [])

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons
        (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
  (cond
    (= k 0)
      1
    :else
      (* n (power n (dec k)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn fib [n]
  (cond
    (or (= n 0) (= n 1))
      n
    :else
      (+ (fib (dec n)) (fib (- n 2)))))

(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (<= up-to 0)
      '()
      (let [new-up-to (dec up-to)]
        (cons new-up-to (my-range new-up-to)))))

(my-range -1)  ;=> ()
(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (= 0 (count a-seq))
    (cons '() a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (= 0 (count a-seq))
    (cons '() a-seq)
    (conj (inits (take (dec (count a-seq)) a-seq)) a-seq))
  )

(tails [])
(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))


(defn rotations [a-seq]
  (let [comp (fn [f] (fn [x y] (f (count x) (count y))))
        i (sort (comp <) (inits a-seq))
        t (sort (comp >) (tails a-seq))]
    (seq (set (map (fn [x y] (concat x y)) t i)))
  )
)

(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (if (< 0 (count a-seq))
    (let [the-key (first a-seq)
          current-value (get freqs the-key)
          new-freqs (if (not= nil current-value)
                      (assoc freqs the-key (inc current-value))
                      (assoc freqs the-key 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))
    freqs))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]

  (if (not-empty a-map)
    (let [x (first a-map)
          k (key x)
          v (val x)]
      (concat (repeat v k) (un-frequencies (rest a-map))))
    '())
  )

(un-frequencies {:a 1, :b 2})
(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (if (and (> n 0) (not-empty coll))
    (cons (first coll) (my-take (dec n) (rest coll)))
    '()))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (and (> n 0) (not-empty coll))
    (my-drop (dec n) (rest coll))
    coll))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]
    ))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]
(halve '(1))         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq))
        '()
      (nil? first-a)
        b-seq
      (nil? first-b)
        a-seq
      (< first-a first-b)
        (cons first-a (seq-merge (rest a-seq) b-seq))
      (> first-a first-b)
        (cons first-b (seq-merge a-seq (rest b-seq))))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (if
    (< (count a-seq) 2)
      a-seq
      (let [halves (halve a-seq)]
        (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(merge-sort [])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

(inits [1 2 3 4])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

