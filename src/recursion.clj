(ns recursion)
(use '[clojure.tools.trace])

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )

(defn singleton? [coll]
  ( if ( and (not (empty? coll)) (empty? (rest coll)))
    true
    false)
  )

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll)))
  )

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq)))))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2)
  )

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq))))
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))))
  )

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq) ))
  )

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()
   )
  )

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq
   )
  )

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or  (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
   :else false
   )
  )

(defn my-map [f seq-1 seq-2]
  (cond
   (and (empty? seq-1) (empty? seq-2)) '()
   (or  (empty? seq-1) (empty? seq-2)) '()
   :else
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
   )
  )

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k))))
  )

(defn fib [n]
  (cond
   (== n 1) 1
   (== n 0) 0
   :else (+ (fib (- n 1)) (fib (- n 2)))
   )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if ((complement pos?) how-many-times)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
   )
  )
(defn my-range [up-to]
  (cond
   (<= up-to 0) '()
   (pos? up-to) (cons (dec up-to) (my-range (dec up-to)))
   )
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))
   )
  )

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
  )

(defn rotations_helper [head tail]
  (let [new-head (rest head)
        new-tail (concat tail (vector (first head)))]
    (
     if (empty? head)
      '()
      (concat (vector (concat head tail)) (rotations_helper new-head new-tail))))
  )

(defn rotations [a-seq]
  (if ( empty? a-seq)
    '(())
    (rotations_helper a-seq '())
   )
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          count (get freqs elem)
          new-freqs (if (nil? count)
                      (assoc freqs elem 1)
                      (assoc freqs elem (inc count)))]
      (my-frequencies-helper new-freqs (rest a-seq))))
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  (if ( empty? a-map)
    '()
    (let [pair (first a-map)
          [elem count] pair]
      (concat (repeat count elem) (un-frequencies (rest a-map)))))
  )

(defn my-take [n coll]
  (cond
   (empty? coll) '()
   (= n 0) '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))
   )
  )

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (= n 0) coll
   :else (my-drop (dec n) (rest coll))
   )
  )

(defn halve [a-seq]
  (let [size (count a-seq)
        half (int (/ size 2))]
    (cons (my-take half a-seq) (vector (my-drop half a-seq))))
  )

(defn seq-merge-helper [final a-seq b-seq]
  (cond
   (empty? a-seq) (concat final b-seq)
   (empty? b-seq) (concat final a-seq)
   :else (let [a-first (first a-seq)
               b-first (first b-seq)]
           (if (< a-first b-first)
             (seq-merge-helper (conj final a-first) (rest a-seq) b-seq)
             (seq-merge-helper (conj final b-first) a-seq (rest b-seq))))
   )
  )
(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq)
  )

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [[f-half s-half] (halve a-seq)]
      (seq-merge (merge-sort f-half) (merge-sort s-half)))
   )
  )

(defn ismonotonic? [a-seq]
  (let [sorted-a-seq (merge-sort a-seq)]
    (if (or (= a-seq sorted-a-seq) (= a-seq (reverse sorted-a-seq)))
      true
      false))
  )

(defn largest-monotonic [a-seq]
  (let [monotonics-list (take-while ismonotonic? (inits a-seq))]
    (nth monotonics-list (dec (count monotonics-list))))
  )

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic (largest-monotonic a-seq)
          size (count monotonic)]
      (cons monotonic (split-into-monotonics (drop size a-seq)))))
  )

(defn add2 [elem a-list-of-lists]
  (if (empty? a-list-of-lists)
    '()
    (cons (cons elem (first a-list-of-lists)) (add2 elem (rest a-list-of-lists))))
  )

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat (fn [some-set]
           (add2 (first some-set) (permutations (rest some-set))))
         (rotations a-set))
   )
  )


(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [pset (powerset (rest a-set))
          elem (first a-set)]
      (concat (map #(conj % elem) pset ) pset))
   )
)
