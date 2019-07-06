(ns recursion)

(defn product [coll]
  (if (empty? coll)
   1
   (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))


(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (max (first coll) (max-element (rest coll))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (seq-max coll (longest-sequence (rest coll))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not (= (count a-seq) (count b-seq))) false
   (empty? a-seq) true
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
   (zero? k) 1
   (= 1 k) n
   :else (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (not (pos? how-many-times)) '()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons (sequence a-seq) (tails (rest a-seq)))))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())



(defn inits [a-seq-initial]
  (let [a-seq (reverse a-seq-initial)]
    (cond
     (empty? a-seq) '(())
     :else (cons (sequence a-seq-initial) (map reverse (tails (rest a-seq)))))))


(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))


(defn rotations [a-seq]
  (let [recFunc (fn curFunc [seq-1 seq-2]
          (cond
           (empty? seq-2) '()
           :else (cons
                  seq-1
                  (curFunc
                   (concat (rest seq-1) [(first seq-1)])
                   (rest seq-2)))))]

    (cond
     (empty? a-seq) '(())
     :else (recFunc a-seq a-seq))))

(defn my-frequencies [a-seq]
  (cond
   (empty? a-seq) {}
   :else (let [currentValue (first a-seq)
               currentMap (my-frequencies (rest a-seq))]
             (assoc
               currentMap
               currentValue
               (if
                 (contains? currentMap currentValue) (inc (get currentMap currentValue)) 1)))))


(my-frequencies [:a "moi" :a "moi" "moi" :a 1])


(defn un-frequencies [a-map]
  (cond
   (empty? a-map) ()
   :else (let [firstKey (first (map key a-map))
               firstValue (first (map val a-map))]
           (concat (repeat firstValue firstKey) (un-frequencies (rest a-map))))))

(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (cond
   (= n 0) ()
   (empty? coll) ()
   :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) ()
   (= n 0) coll
   :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [halfCount (int (/ (count a-seq) 2))]
    (vector (my-take halfCount a-seq) (my-drop halfCount a-seq))))
  ;=> [(1 2) (3 4)]

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])


(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (cond
   (= 1 (count a-seq)) a-seq
   (empty? a-seq) ()
   :else (let [halved1 (vec (first (halve a-seq)))
               halved2 (vec (second (halve a-seq)))]
           (seq-merge (merge-sort halved1) (merge-sort halved2)))))

(defn split-into-monotonics-helper [cur-seq a-seq func]
  (cond
   (empty? a-seq)  (list cur-seq)
   (and
    (not (nil? func))
    (not (empty? cur-seq))
    (func (last cur-seq) (first a-seq)))
     (cons cur-seq (split-into-monotonics-helper () a-seq nil))


   :else (split-into-monotonics-helper
          (conj (vec cur-seq) (first a-seq))
          (rest a-seq)
          (cond
           (not (nil? func)) func
           (empty? cur-seq) nil
           (= (last cur-seq) (first a-seq)) nil
           (< (last cur-seq) (first a-seq)) >
           :else <))))

(defn split-into-monotonics [a-seq]
  (map (fn [x] (reverse (into '() x))) (split-into-monotonics-helper '() a-seq nil)))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

