(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (let [restcoll (rest coll)]
    (if (empty? restcoll)
      (first coll)
      (my-last restcoll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))
;(defn max-element [a-seq]
;  (if (empty? a-seq)
;    nil
;    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   ))

(defn power [n k]
  (cond
   (< k 0)
     (/ 1 (power n (* k -1)))
   (= k 0)
     1
   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0)
     0
   (= n 1)
     1
   :else
     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reverse-list (fn [list] (reverse list))]
    (map reverse-list (tails (reverse a-seq)))))

(defn first-n [a-seq n]
  (if (= n 0)
    '()
    (cons (first a-seq) (first-n (rest a-seq) (dec n)))))

(defn rest-n [a-seq n]
  (if (= n 0)
    a-seq
    (rest-n (rest a-seq) (dec n))))

(defn rotations [a-seq]
  (let [rotate-seq (fn [n] (concat (rest-n a-seq n) (first-n a-seq n) ))]
    (map rotate-seq (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-item (first a-seq)]
      (my-frequencies-helper (assoc freqs first-item (inc (get freqs first-item 0))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies2 [a-map]
  (flatten (map (fn [k] (repeat (get a-map k) k)) (keys a-map))))

(defn un-frequencies [a-map]
  (let [first-item (first a-map)]
    (if (empty? a-map)
      '()
      (concat (repeat (get first-item 1) (get first-item 0)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

