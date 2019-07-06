(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? ( rest coll))))

(defn my-last [coll]
  (if (empty? (next coll))
    (first coll)
    (recur (next coll))))

#_(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [srt-coll (sort a-seq)]
      (last srt-coll))))

; recursive version.
(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

#_(defn longest-sequence [a-seq]
  (reduce seq-max nil a-seq))

; explicit recursive version:
(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

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
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn first-in [val seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) 0
    (= (first seq-1) val) 1
    (= (first seq-2) val) 2
    :else (first-in val (rest seq-1) (rest seq-2))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    '()
    (cons
     (f (first a-seq) (first b-seq))
     (my-map f (rest a-seq) (rest b-seq)))))

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

#_(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
      (take (count a-seq)
            (iterate #(concat (rest %) [(first %)]) a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-count (inc (or (get freqs elem) 0))
         new-freqs (assoc freqs elem new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (not (empty? a-map))
    (let [pair (first a-map)
          elem (first pair)
          n (second pair)]
      (concat (repeat n elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (zero? n) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid-point (int (/ (count a-seq) 2))]
    (vector (my-take mid-point a-seq) (my-drop mid-point a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (< (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (->>
     (halve a-seq)
     (map merge-sort)
     (apply seq-merge))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
