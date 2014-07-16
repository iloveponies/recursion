(ns recursion
  (:require [clojure.math.numeric-tower :as math]))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (letfn [(max-acc [a-seq max]
                   (if (empty? a-seq)
                     max
                     (if (> (first a-seq) max)
                       (max-acc (rest a-seq) (first a-seq))
                       (max-acc (rest a-seq) max))))]
    (max-acc (rest a-seq) (first a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
         seq-2))

(defn longest-sequence [a-seq]
  (letfn [(long-acc [a-seq max]
                   (if (empty? a-seq)
                     max
                     (if (seq-max (first a-seq) max)
                     (long-acc (rest a-seq) (first a-seq))
                     (long-acc (rest a-seq) max))))]
    (long-acc (rest a-seq) (first a-seq))))

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
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '() ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq) ))

(defn seq= [seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) true
    (= (first seq-1) (first seq-2)) (seq= (rest seq-1) (rest seq-2))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= k 1) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1) n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '()
    (concat (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) a-seq
    (conj (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) (seq '())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))


(defn rotations [a-seq]
  (concat (rest a-seq) (vector (first a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (#(if (nil? %)
     (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)
     (my-frequencies-helper (assoc freqs (first a-seq) (inc %)) (rest a-seq))))
    (get freqs (first a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-drop [n coll]
  (cond
   (or (= n 0) (empty? coll)) coll
   :else (my-drop (dec n) (rest coll))))

(defn my-take [n coll]
  (cond
   (or (= n 0) (empty? coll)) []
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn halve [a-seq]
  ((fn [x] (vector (my-take x a-seq) (my-drop x a-seq)))(int (/ (count a-seq) 2))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (peek a-seq) (peek b-seq)) (conj (seq-merge a-seq (pop b-seq)) (peek b-seq) )
   :else (conj (seq-merge (pop a-seq) b-seq) (peek a-seq))))

(defn merge-sort [a-seq]
  (cond
   (or (empty? a-seq) (singleton? a-seq)) a-seq
   :else (let [[x y] (halve a-seq)]
           [(merge-sort x) (merge-sort y)])))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

