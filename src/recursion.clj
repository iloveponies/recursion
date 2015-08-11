(ns recursion)

(defn product [coll]
  (if (clojure.core/empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;2.
;
;(product [1 2 4])
;(product (cons 1 (cons 2 (cons 4 '()))))
;(* 1 (product (cons 2 (cons 4 '()))))
;(* 1 (* 2 (product (cons 4 '()))))
;(* 1 (* 2 (* 4 (product '()))))
;(* 1 (* 2 (* 4 1)))
;...
;

(defn singleton? [coll]
  (and 
    (clojure.core/empty? (rest coll)) 
    (not (clojure.core/empty? coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (clojure.core/empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (clojure.core/empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (clojure.core/empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (clojure.core/empty? a-seq)
    a-seq 
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (clojure.core/empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (clojure.core/empty? a-seq) a-seq 
    (pred? (first a-seq)) 
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (clojure.core/empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (clojure.core/empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (clojure.core/empty? seq-2) seq-2
    :else (cons 
            (f (first seq-1) (first seq-2)) 
            (my-map f (rest seq-1) (rest seq-2))))) 

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond 
    (zero? n) 0
    (== 1 n) 1
    :else (+ 
            (fib (dec n)) 
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
    (> 1 how-many-times) '()
    :else (cons what-to-repeat 
                (my-repeat 
                  (dec how-many-times) 
                  what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (clojure.core/empty? a-seq) ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (clojure.core/empty? a-seq) 
    '(())
    (rest (reverse (map concat (tails a-seq) (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (clojure.core/empty? a-seq)
    freqs
    (let [item-count
          (get freqs (first a-seq))
          new-freqs 
          (assoc freqs (first a-seq) (if (nil? item-count) 1 (+ item-count 1)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (clojure.core/empty? a-map)
    '()
    (let [item-to-repeat (first (first a-map))
          times-to-repeat (last (first a-map))]
      (concat 
        (repeat times-to-repeat item-to-repeat)
        (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (nil? (first coll)))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  (let [middle-index (int (/ (count a-seq) 2))]
    (vector (my-take middle-index a-seq) (my-drop middle-index a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond 
      (and (clojure.core/empty? a-seq) (empty? b-seq)) '()
      (and (clojure.core/empty? a-seq) (not (empty? b-seq))) b-seq
      (and (not (clojure.core/empty? a-seq)) (empty? b-seq)) a-seq
      (<= first-a first-b) (cons first-a (seq-merge (rest a-seq) b-seq))
      :else (cons first-b (seq-merge a-seq (rest b-seq))))))     


(defn merge-sort [a-seq] 
  (if (or (= (count a-seq) 0) (= (count a-seq) 1))
    a-seq
    (let [split-seq (halve a-seq)]
      (seq-merge (merge-sort (first split-seq)) (merge-sort (last split-seq))))))
  

;(defn split-into-monotonics [a-seq]
;    (if (<= (count a-seq) 3)
;      a-seq
;      (let [split-seq 
;        (if (== 0 (mod (count a-seq) 2))
;          (halve a-seq)
;          (let [middle-index (+ (int (/ (count a-seq) 2)) 1)]
;            (vector (take middle-index a-seq) (drop middle-index a-seq))))]
;        (vector 
;          (split-into-monotonics (first split-seq))
;          (split-into-monotonics (last split-seq))))))

(defn split-into-monotonics [a-seq] 
  (let [n-to-take (if (= 0 (mod (count a-seq) 2)) 2 3)]
    (if (<= (count a-seq) 2)
      (vector a-seq)
      (cons 
        (take n-to-take a-seq) 
        (split-into-monotonics (drop n-to-take a-seq))))))

(defn permutations [a-set]
  (if (<= (count a-set) 2)
    (rotations a-set)
    (map 
      (fn [coll] 
        (let [f (fn [m] (cons (first coll) m))]
          (map
            f
            (permutations (rest coll)))))
      (rotations a-set))))

(defn powerset [a-set]
  [:-])

