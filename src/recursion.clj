(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (if (= elem (first a-seq)) true (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq))) '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq)) a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq)) true
    (if (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)) false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 1
    (if (< 1 k) (* n (power n (dec k))) n)))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (singleton? a-seq) (seq [a-seq '()])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (rest (map concat (reverse (tails a-seq)) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper
      (assoc freqs (first a-seq)
        (if (boolean (get freqs (first a-seq)))
          (inc (get freqs (first a-seq)))
          1))
      (rest a-seq))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (first (rest (first a-map))) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (<= n 0) '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (- (count a-seq) (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (if (empty? a-seq) b-seq a-seq)
    (let [condition (< (first a-seq) (first b-seq))]
      (cons
        (if condition (first a-seq) (first b-seq))
        (if condition (seq-merge (rest a-seq) b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [b-seq (last (my-take-while
            (fn [init] (or (= init (merge-sort init)) (= init (reverse (merge-sort init)))))
            (inits a-seq)))]
    (cons b-seq
          (split-into-monotonics (my-drop (count b-seq) a-seq))))))

(defn permutations-helper [result a-set]
 (if (empty? a-set) result
  (map
    (fn [rotation]
      (permutations-helper (cons (first rotation) result) (rest rotation)))
      (rotations a-set))))

(defn disassemble [n permutations]
  (if (= 2 n)
    (apply concat permutations)
    (disassemble (dec n) (apply concat permutations))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (disassemble (count a-set) (permutations-helper '() a-set))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
    (set (map set (apply concat (map inits (permutations a-set)))))))





