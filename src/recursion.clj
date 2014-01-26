;-----------------------------------------------------------------------
(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

;; Non-recursive way
;(defn product [coll]
;  (apply * coll))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

;; I'm assuming they want me to use recursion to implement these
;; functions since Clojure already has functions to do these things.

(defn my-last [coll]
  (if (nil? (next coll))
    (first coll)
    (recur (next coll))))

(defn max-element [a-seq]
  (loop [max (first a-seq) a-seq (next a-seq)]
    (let [item (first a-seq)]
      (if (nil? item)
        max
        (let [next-max (if (< max item)
                         item
                         max)]
          (recur next-max (next a-seq)))))))

;; Non-recursive way
; (defn max-element [a-seq]
;   (first (reverse (sort a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (reduce seq-max nil a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [item (first a-seq)
          filtered-seq (my-filter pred? (rest a-seq))]
      (if (pred? item)
        (conj filtered-seq item)
        filtered-seq))))

(comment
  ;; Using reduce instead of explicit recur
  (defn my-filter [pred? a-seq]
    (let [f (fn [b-seq item]
              (if (pred? item)
                (conj b-seq item)
                b-seq))]
      (reduce f [] a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (loop [a-seq a-seq b-seq []]
    (let [item (first a-seq)]
      (if (or (empty? a-seq) (-> item pred? not))
        b-seq
        (recur (rest a-seq) (conj b-seq item))))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (-> a-seq first pred? not))
    a-seq
    (recur pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (loop [seq-1 seq-1 seq-2 seq-2 result []]
    (if (or (empty? seq-1) (empty? seq-2))
      result
      (let [new-item (f (first seq-1) (first seq-2))
            new-result (conj result new-item)]
        (recur (rest seq-1) (rest seq-2) new-result)))))

(comment
  ;; Without recursion
  (defn power [n k]
      ((apply comp (repeat k (partial * n))) 1)))

(comment
  ;; Non-tail-recursive version
  (defn power [n k]
    (if (= k 0)
      1
      (* n (power n (dec k))))))

;; Tail-recursive version
(defn power [n k]
  (loop [k k result 1]
    (if (= k 0)
      result
      (recur (dec k) (* n result)))))

;; Tree-recursive; ouch!
(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(comment
  ;; Tail-recursive version; efficient
  (defn fib [n]
    (loop [n n a 0 b 1]
      (if (= n 0)
        a
        (recur (dec n) b (+ a b))))))

(comment
  ;; Without using explicit recursion
  (defn fib [n]
    (let [next-fib-pair (fn [[a b]] [b (+ a b)])]
      (first (nth (iterate next-fib-pair [0 1]) n)))))

(comment
  ;; Using reduce instead of explicit recursion
  (defn fib [n]
    (let [next-fib-pair (fn [[a b] _] [b (+ a b)])]
      (first (reduce next-fib-pair [0 1] (range n))))))

(comment
  ;; Not tail-recursive
  (defn my-repeat [n item]
    (if (<= n 0)
      []
      (let [the-seq (my-repeat (dec n) item)]
        (conj the-seq item)))))

;; Tail-recursive
(defn my-repeat [n item]
  (loop [a-seq [] n n]
    (if (<= n 0)
      a-seq
      (recur (conj a-seq item) (dec n)))))

(defn my-range [up-to]
  (loop [a-seq [] n (dec up-to)]
    (if (< n 0)
      a-seq
      (recur (conj a-seq n) (dec n)))))

(comment
  ;; More efficient version
  (defn tails [a-seq]
    (take (inc (count a-seq)) (iterate rest a-seq))))

(defn map-range [f a-seq]
  (map #(f % a-seq) (range (inc (count a-seq)))))

(defn tails [a-seq]
  (map-range drop a-seq))

(defn inits [a-seq]
  (map-range take a-seq))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

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

