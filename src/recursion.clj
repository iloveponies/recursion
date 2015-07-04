(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil (if (= (count a-seq) 1) (first a-seq) (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (empty? a-seq) false
   (empty? b-seq) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f(first seq-1)(first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1 (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib(- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '() (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '() (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(()) (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (= (apply + (vals freqs)) (count a-seq))
    {} (merge-with + (my-frequencies-helper {} (rest a-seq)) {(first a-seq) 1})))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map a-seq]
  (let [[frst-key frst-value] (first a-map)
        done? (fn [m] (empty? a-map))]
    (if (done? a-map) '() (concat (repeat frst-value frst-key) (un-frequencies-helper (rest a-map) '())))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map '()))

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1)) '() (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  (let [seq-length (count a-seq)
        length1 (/ seq-length 2)
        length2 (if (even? seq-length) length1 (- length1 1))]
    [(my-take length1 a-seq) (my-drop length2 a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a (first a-seq)
               b (first b-seq)]
           (if (< a b)
             (concat [a] (seq-merge (rest a-seq) b-seq))
             (concat [b] (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (let [a-seq-length (count a-seq)]
    (if (< a-seq-length 2)
    a-seq
    (let [[a b] (map merge-sort(halve a-seq))] (seq-merge a b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  ())

(defn powerset [a-set]
  [:-])

