(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (first coll)
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq)
         (or (max-element (rest a-seq))
             0))))

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])      

(defn seq-max-r [orig-seq-1 orig-seq-2 seq-1 seq-2]
  (let [a1 (or (first seq-1) 0)
        a2 (or (first seq-2) 0)]
    (if (= a1 a2)
      (seq-max-r orig-seq-1 orig-seq-2 (rest seq-1) (rest seq-2))
      (if (> a1 a2)
        orig-seq-1
        orig-seq-2))))

(defn seq-max [seq-1 seq-2]
  (println "seq-max")
  (println seq-1)
  (println seq-2)
  (seq-max-r seq-1 seq-2 seq-1 seq-2))

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [seq-of-seq]
  (if (empty? seq-of-seq)
    nil
    (seq-max (first seq-of-seq)
             (longest-sequence (rest seq-of-seq)))))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])

(longest-sequence [ [1 2] [1 2 3] [1 2 3 4 5] [1 2 3] [1 2] [1]])


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()

(defn only-numbers? [coll]
  (cond
   (empty? coll) true
   (number? (first coll)) (only-numbers? (rest coll))
   :else false))

(only-numbers? [1 2 3 4 5])
(only-numbers? [1 2 :a 4 5])

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(sequence-contains? :c [:a :c :e])
(sequence-contains? :b [:a :c :e])

(defn my-take-while [pred? a-seq]
  (cond
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))

(my-take-while odd? [1 3 5 6 7])

(defn my-drop-while [pred? a-seq]
  (cond
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(my-drop-while odd? [1 3 5 6 7])

(defn first-in [val seq-1 seq-2]
  (cond
   (and (empty? seq-1) (empty? seq-2)) 0
   (= (first seq-1) val) 1
   (= (first seq-2) val) 2
   :else (first-in val (rest seq-1) (rest seq-2))))

(first-in 5 [1 3 5] [2 4 6])
(first-in 4 [1 3 5] [2 4 6])
(first-in 10 [1 3 5] [2 4 6])

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(seq= [1 2 4] '(1 2 4))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])
(map vector [1 2 3] [:a :b :c])

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(indexed [:a :b :c])

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(consecutives [:a :b :c])

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(factorial 10)

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(power 2 2)

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 10)

(defn my-repeat [how-many-times what-to-repeat]
  (if (zero? how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times)
                                    what-to-repeat))))

(my-repeat 3 "lol")

(defn my-range [up-to]
  (if (zero? up-to)
    nil
    (conj (vector (my-range (dec up-to)))
          up-to)))

(defn rangese
  [start end]
  (if (>= start end)
                  '()
                  (cons (dec end) (rangese start (dec end)))))

(rangese 0 3)
(rangese 0 0)

(defn my-range [up-to]
  (rangese 0 up-to))

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails-iter [a-seq i]
  (if (> i (count a-seq))
    '()
    (cons (drop i a-seq)
          (tails-iter a-seq (inc i)))))

(defn tails [a-seq]
  (tails-iter a-seq 0))

(tails [1 2 3 4])

(defn inits-iter [a-seq i]
  (if (> i (count a-seq))
    '()
    (cons (take i a-seq)
          (inits-iter a-seq (inc i)))))

(defn inits [a-seq]
  (inits-iter a-seq 0))

(inits [1 2 3 4])

(defn rotation [a-seq i]
  (concat (drop i a-seq)
          (take i a-seq)))

(rotation [1 2 3 4 5] 0)

(defn rotations-iter [a-seq i]
  (if (>= i (count a-seq))
    '()
    (cons (rotation a-seq i)
          (rotations-iter a-seq (inc i)))))

(defn rotations [a-seq]
  (rotations-iter a-seq 0))

(rotations [1 2 3 4 5])

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-count (inc (get freqs elem 0))
          new-freqs (assoc freqs elem elem-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies [:a "moi" :a "moi" "moi" :a 1])

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [elem-count (first a-map)
          elem (first elem-count)
          num (second elem-count)]
      (concat (repeat num elem)
              (un-frequencies (rest a-map))))))

(un-frequencies {:a 3 :b 2 "^_^" 1})

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(my-take 2 [1 2 3 4])
(my-take 4 [:a :b])

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n)
             (rest coll))))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq)
     (my-drop n a-seq)]))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])

(defn seq-merge-helper [result a-seq b-seq]
  (cond
   (empty? a-seq) (concat result b-seq)
   (empty? b-seq) (concat result a-seq)
   :else (let [a (first a-seq)
               b (first b-seq)
               new-result (if (< a b)
                            (conj result a)
                            (conj result b))
               new-a (if (< a b) (rest a-seq) a-seq)
               new-b (if (< a b) b-seq (rest b-seq))]
           (seq-merge-helper new-result new-a new-b))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(seq-merge [4] [1 2 6 7])
(seq-merge [1 5 7 9] [2 2 8 10])

(defn merge-sort [a-seq]
  (if (= 1 (count a-seq))
    a-seq
    (let [halves (halve a-seq)
          first (first halves)
          second (second halves)]
      (seq-merge (merge-sort first)
                 (merge-sort second)))))

(merge-sort [4 2 3 1])
(merge-sort [5 3 4 17 2 100 1])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

