(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
    (if (empty? (rest coll)) (first coll) (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq (if (pred? (first a-seq))
                             (cons (first a-seq) (my-filter pred? (rest a-seq)))
                             (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq) false
     (= (first a-seq) elem) true
     :else (sequence-contains? elem (rest a-seq ))))

(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq) a-seq
     (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else [] ))

(defn my-drop-while [pred? a-seq]
   (cond
     (empty? a-seq) a-seq
     (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
     :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (=(first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1 (* n (power n (- k 1)))))

(defn fib [n]
  (cond
     (= n 0) 0
     (= n 1) 1
     :else ( + (fib (- n 1))  (fib (- n 2)))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) [] (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]] (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) [[]]
  (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
   (let [key (first a-seq)
         new-freqs (if (contains? freqs key)
                     (update-in freqs [key] inc)
                     (assoc freqs key 1))]
    (my-frequencies-helper new-freqs (rest a-seq))
  )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
  (let [[el rep] (first a-map)]
  (concat (repeat rep el) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll)) [] (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll)) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [splitp (int (/ (count a-seq) 2))]
    [(my-take splitp a-seq) (my-drop splitp a-seq)]))

(defn seq-merge [a-seq b-seq]
    (let [[a] a-seq [b] b-seq]
      (cond
       (nil? a) b-seq
       (nil? b) a-seq
       :else
        (if(< a b)
          (cons a (seq-merge (rest a-seq) b-seq))
          (cons b (seq-merge a-seq (rest b-seq)))
    ))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) a-seq
    (let [[h1 h2] (halve a-seq)]
      (seq-merge (merge-sort h1) (merge-sort h2))
  )))

(defn monotonic? [a-seq]
   (or (apply <= a-seq) (apply >= a-seq)
  ))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) a-seq
  (let [prefs (inits a-seq)
     cur (last (take-while monotonic? (rest prefs)))]
    (cons cur (split-into-monotonics (drop (count cur) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set) [[]]
        ; for every first char, we permutate all remaining chars
        (mapcat (fn[x]
                ; x is one rotation of the current seq
                ; y is are all permutations of remaining seq
               (map (fn[y] (cons (first x) y)) (permutations (rest x)))
         ) (rotations a-set))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
    (set (map set (cons a-set (mapcat (fn[x] (powerset (rest x))) (rotations a-set))
    )))))
