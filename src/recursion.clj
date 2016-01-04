(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
   (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
        (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
        (if (empty? a-seq)
          a-seq
          (if (pred? (first a-seq))
            (cons (first a-seq) (my-filter pred? (rest a-seq)))
            (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
   false
   (= (first a-seq) elem)
   true
   :else
   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   a-seq
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   a-seq
   ))

(defn seq= [a-seq b-seq]
  (let [a (seq a-seq)
        b (seq b-seq)]
    (= a b)))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2))
   ()
   (cons
    (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
    (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n)
    0
   (= n 1)
   1
   :else
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
  ()
  (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (seq a-seq) (inits  (reverse (rest (reverse a-seq)))))))

(defn my-rotations [a-seq n]
         (cond
          (<= n 0) (list a-seq)
          :else (cons a-seq
                 (my-rotations
                  (concat (rest a-seq) [(first a-seq)])
                  (dec n)))))

(defn rotations [a-seq]
  (my-rotations a-seq (dec (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          freq (get freqs k 0)
          new-freqs (assoc freqs k (inc freq))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
      a-map
      (let [k (first (vals a-map))
            x (first (keys a-map))]
        (concat (repeat k x)
      (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
     ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
    (let [n (int (/ (count a-seq) 2))]
      [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? b-seq) a-seq
   (empty? a-seq) b-seq
  :else  (let [a (first a-seq)
        b (first b-seq)]
    (if (< a b)
      (cons a (seq-merge (rest a-seq) b-seq))
      (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (= (count a-seq) 1) (= (count a-seq) 0))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
