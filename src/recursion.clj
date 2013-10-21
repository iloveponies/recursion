(ns recursion)

(defn product [coll]
  (if(empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(product [1 2 4]) ;=> 8

(defn singleton? [coll]
  (if(empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if(or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if(> (count seq-1) (count seq-2))
  seq-1
  seq-2))


(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (if(empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
    (not (= elem elem (first a-seq))) (sequence-contains? elem (rest a-seq))
    :else true))


(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (cons (first a-seq) (rest a-seq))))


(defn seq= [a-seq b-seq]
  (cond
   (not (== (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))


(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
  1
  (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(tails [1 2 3 4])


(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () '())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))




(defn rotations [x]
  (if (seq x)
  (map
    (fn [n _]
      (lazy-cat (drop n x) (take n x)))
    (iterate inc 0) x)
   (list ())))

; ei pysty voida kyetä


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [frequency (if (contains? freqs (first a-seq))
                      (inc (get freqs (first a-seq)))
                      1)]
      (my-frequencies-helper (assoc freqs (first a-seq) frequency) (rest a-seq)))))



(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (apply concat (map (fn [[k v]] (repeat v k)) (set a-map))))


(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))



(defn my-drop [n coll]
  (if (>= n (count coll))
    '()
    (if (zero? n)
      (cons (first coll) (my-drop n (rest coll)))
      (my-drop (dec n) (rest coll)))))



(defn halve [a-seq]
  (let [division (int (/ (count a-seq) 2))]
    (conj [(my-take division a-seq)] (my-drop division a-seq))))


(defn seq-merge [a-seq b-seq]
 (if (and (empty? a-seq) (empty? b-seq))
   '()
   (cond

    (and (seq a-seq) (or (empty? b-seq) (< (first a-seq) (first b-seq))))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))

    (or (empty? a-seq) (<= (first b-seq) (first a-seq)))
    (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))


(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (seq-merge
     (merge-sort (get (halve a-seq) 0))
     (merge-sort (get (halve a-seq) 1)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])






