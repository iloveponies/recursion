(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll))
           (empty? (rest coll))) true false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
    '()
   (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not (= (first a-seq) (first b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (zero? how-many-times) '()
   (neg? how-many-times) '()
   :else (cons what-to-repeat
               (my-repeat
                (dec how-many-times)
                what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (and (empty? a-seq) (empty? (rest a-seq)))
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn counter-helper [n a-seq]
  (if (zero? n)
    '()
    (let [new-seq (concat (rest a-seq) [(first a-seq)])] ;käännä
      (cons a-seq (counter-helper (dec n) new-seq)))))

(defn rotations [a-seq]
  (if (zero? (count a-seq))
    (cons a-seq '())
  (counter-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (= (count a-seq) 0)
    freqs
    (let [new-freqs
      (if (get freqs (first a-seq))
        (assoc freqs (first a-seq)
          (inc (get freqs (first a-seq))))
        (assoc freqs (first a-seq) 1))]
     (my-frequencies-helper new-freqs (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? (first a-map))
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (zero? n)
    '()
    (if (= (count coll) 0)
      '()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if
    (zero? n) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (cons (my-take half a-seq) [(my-drop half a-seq)])))

(defn seq-merge-helper [a b]
  (cond
   (empty? a) b
   (empty? b) a
   (< (first a) (first b))
     (cons (first a) (seq-merge-helper (rest a) b))
   :else
     (cons (first b) (seq-merge-helper (rest b) a))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq))

(defn merge-sort [a-seq]
  (let [[a b] (halve a-seq)]
    (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

