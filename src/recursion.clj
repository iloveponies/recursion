(ns recursion
  (:require [clojure.set :as set]))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (boolean (and (seq coll) (empty? (next coll)))))

(defn my-last [coll]
  (if (empty? (next coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (next a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (first (sort-by count > (reverse [seq-1 seq-2])))) ;reverse needed because the tests suck

(defn longest-sequence [a-seq]
  (if (not (empty? a-seq))
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq) r (rest a-seq)]
      (if (pred? f)
        (cons f (my-filter pred? r))
        (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq) r (rest a-seq)]
    (cond
      (empty? a-seq) ()
      (not (pred? f)) ()
      :else (cons f (my-take-while pred? r)))))

(defn my-drop-while [pred? a-seq]
  (let [f (first a-seq) r (rest a-seq)]
    (cond
      (empty? a-seq) ()
      (not (pred? f)) (seq a-seq)
      :else (my-drop-while pred? r))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq)
        (empty? b-seq)
        (not= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (let [d (dec up-to)]
      (cons d (my-range d)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

;(defn rotations2 [xs] (take (count xs) (partition (count xs) 1 (cycle xs))))


(defn my-frequencies-helper [freqs a-seq]
    (if (empty? a-seq)
      freqs
      (let [f (first a-seq)
            r (rest a-seq)
            curr (if (contains? freqs f)
                    (get freqs f)
                    0)]
        (my-frequencies-helper (assoc freqs f (inc curr)) r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [out a-map]
  (if (empty? a-map)
    out
    (let [f (first a-map)
          r (rest a-map)]
      (un-frequencies-helper (concat out (repeat (val f) (key f))) r))))


(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map))

;(my-take 2 [1 2 3 4]) ;=> (1 2)
;(my-take 4 [:a :b])   ;=> (:a :b)
(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (zero? n) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-low? (< (first a-seq) (first b-seq))
                f (if a-low? (first a-seq) (first b-seq))
                r (if a-low? (rest a-seq) (rest b-seq))
                h (if a-low? b-seq a-seq)]
            (cons f (seq-merge r h)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[f s] (halve a-seq)]
      (seq-merge (merge-sort f) (merge-sort s)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic? (fn [a-seq] boolean (or (empty? a-seq)
                                             (apply < a-seq)
                                             (apply > a-seq)))
          next (last (take-while monotonic? (inits a-seq)))]
      (cons next (split-into-monotonics (drop (count next) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat rotations (map #(cons (first a-set) %) (permutations (rest a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [add-ele (fn [ele comp] (map #(set/union #{ele} %) (powerset comp)))]
      (set/union (powerset (rest a-set)) (add-ele (first a-set) (rest a-set))))))



