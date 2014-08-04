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
    (or (empty? seq-1) (empty? seq-2)) false
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
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (conj (tails (rest a-seq)) (seq a-seq) )))

(defn inits [a-seq]
  (if (empty? a-seq) '(())
    (conj (inits (reverse (rest (reverse a-seq)))) (seq a-seq))))


(defn rotations [a-seq]
  (reduce (fn [x _] (conj x (concat (rest (peek x)) (vector (first (peek x)))))) [a-seq] (range (dec (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (nil? (get freqs (first a-seq))) (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
   :else (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))))

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
   (not (coll? a-seq)) (seq-merge (vector a-seq) b-seq)
   (not (coll? b-seq)) (seq-merge a-seq (vector b-seq))
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (peek a-seq) (peek b-seq)) (conj (seq-merge a-seq (pop b-seq)) (peek b-seq) )
   :else (conj (seq-merge (pop a-seq) b-seq) (peek a-seq))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (singleton? a-seq) (first a-seq)
   :else (let [[x y] (halve a-seq)]
           (seq-merge (merge-sort x) (merge-sort y)))))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq)) a-seq
    (let [mono (last
                (filter (fn [a-seqn]
                          (or
                           (apply <= a-seqn)
                           (apply >= a-seqn)))
                        (map (fn [x] (my-take x a-seq)) (range 1 (inc (count a-seq))))))]
      (cons mono (split-into-monotonics (my-drop (count mono) a-seq))))))

(defn permutations [a-set]
  (cond
   (empty? a-set) [a-set]
   (singleton? a-set) [a-set]
   :else (for [i (range(count a-set))
          p (permutations (concat (my-take i a-set) (my-drop (inc i) a-set)))]
      (conj p (get (into [] a-set) i)))))

(defn powerset [a-set]
  (cond
   (or (empty? a-set) (singleton? a-set)) #{a-set}
   :else (apply clojure.set/union #{#{}} #{a-set} (map (fn [i]
                            (powerset (clojure.set/union (into #{} (my-take i a-set)) (into #{} (my-drop (inc i) a-set)))))
                          (range (count a-set))))))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

