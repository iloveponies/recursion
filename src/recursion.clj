(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (and (nil? (next coll)) (not (empty? coll))))

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false


(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (if (= (count a-seq) 0)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (>= (first (rest a-seq)) (first a-seq))
        (max-element (rest a-seq))
        (max-element (conj (rest (rest a-seq)) (first a-seq)))))))

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [-1 1 2] [3 4]) ;=> [3 4]


(defn longest-sequence [a-seq]
  (if (= (count a-seq) 0)
    nil
    (if (= (count a-seq) 1)
      (first a-seq)
      (if (= (first a-seq)
             (seq-max
               (first a-seq)
               (first (rest a-seq))))
        (longest-sequence (conj
                            (rest (rest a-seq))
                            (first a-seq)))
        (longest-sequence (rest a-seq))))))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])                 ;=> nil


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(my-filter (fn [x] (> x 0)) [1 2 3 0])

(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons
                            (first a-seq)
                            (my-take-while pred? (rest a-seq)))
  :else
    '()))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
  :else
    a-seq))

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and
      (= 0 (count a-seq))
      (= 0 (count b-seq))
      ) true
    (and
      (= (first a-seq) (first b-seq))
      (and (= 1 (count a-seq)) (= 1 (count b-seq)))
      ) true
    (and
      (= (first a-seq) (first b-seq))
      (not (= 2 (+ (count a-seq) (count b-seq))))
      ) (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(seq= [1 2 nil] [1 2])   ;=> false

(defn my-map [f seq-1 seq-2]
  (if (or
        (empty? seq-1)
        (empty? seq-2))
    '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()


(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (concat
      (vector what-to-repeat)
      (my-repeat (dec how-many-times) what-to-repeat))))

(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (concat (vector (dec up-to)) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list '())
    (cons
      (seq a-seq)
      (tails (rest a-seq)))))

(tails [1 2 3 4])
;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(inits [1 2 3])

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat
               (reverse (tails a-seq))
               (reverse (inits a-seq))))))

(rotations [1 2 3])


(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq)
                               (+ (get freqs (first a-seq)) 1))
                             (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) 1)
                                 (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [curr (first a-map)]
      (concat
        (repeat (last curr) (first curr))
        (un-frequencies (rest a-map))))))

(un-frequencies {:a 3 :b 2 :3 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn take-help [i n coll]
  (if (or (<= n i) (empty? coll))
    ()
    (cons
      (first coll)
      (take-help (inc i) n (rest coll)))))

(defn my-take [n coll]
  (take-help 0 n coll))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (> n (count coll))
    '()
    (reverse (my-take n (reverse coll)))))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [idx (int (/ (count a-seq) 2))]
    (concat
      (vector (my-take idx a-seq))
      (vector (my-drop (- (count a-seq) idx) a-seq)))))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]


(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons
        (first a-seq)
        (seq-merge (rest a-seq) b-seq))
    (> (first a-seq) (first b-seq))
      (cons
        (first b-seq)
        (seq-merge a-seq (rest b-seq)))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)

(seq-merge '(3) [2])

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (= 1 (count a-seq)) a-seq
    :else
      (seq-merge
        (merge-sort (first (halve a-seq)))
        (merge-sort (last (halve a-seq))))))

(merge-sort [])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

