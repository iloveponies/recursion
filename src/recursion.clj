(ns recursion
  (:use clojure.repl))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
;;   helper
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? rst)
      fst
      (max fst (max-element rst)))))

(defn seq-max [seq-1 seq-2]
;;   helper
    (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? rst)
      fst
      (seq-max fst (longest-sequence rst)))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? a-seq)
    a-seq
    (if (pred? fst)
      (cons fst (my-filter pred? rst))
      (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons
                          (first a-seq)
                          (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (not (empty? a-seq)) (empty? b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   (and (empty? a-seq) (empty? b-seq)) true
   :else (seq= (rest a-seq) (rest b-seq))))

(and true false)

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [drop-last (fn [s] (reverse (rest (reverse s))))]
    (if (empty? a-seq)
      (cons a-seq '())
      (cons (seq a-seq) (inits (drop-last a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (rest (map concat (reverse (tails a-seq))  (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          new-freqs (assoc freqs fst (inc (freqs fst 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat
     (apply repeat (reverse (first a-map)))
     (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  [(my-take (quot (count a-seq) 2) a-seq)
   (my-drop (quot (count a-seq) 2) a-seq)])

(defn seq-merge [a-seq b-seq]
  (let [fst (first a-seq)
        lt-fst? (fn [x] (< x fst))]
    (if (empty? a-seq)
      b-seq
      (seq-merge (rest a-seq)
                 (concat (my-take-while lt-fst? b-seq)
                         (list fst)
                         (my-drop-while lt-fst? b-seq))))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[part-1 part-2] (halve a-seq)]
      (seq-merge (merge-sort part-1)
                 (merge-sort part-2)))))

;; Extras
(defn permutations [a-set]
  (let [permut-helper (fn [a-seq]
                        (map
                         (fn [x] (cons (first a-seq) x))
                         (permutations (rest a-seq))))]
    (if (empty? a-set)
      (list '())
      (apply concat (map permut-helper (rotations a-set))))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn powerset [a-set]
  [:-])
