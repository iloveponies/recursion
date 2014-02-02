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

(comment
  ;; The rotate function uses split-at.  Map rotate over range.
  (defn rotations [a-seq]
    (let [rotate (fn [n]
                   (let [[a b] (split-at n a-seq)]
                     (concat b a)))]
      (if (empty? a-seq)
        [[]]
        (map rotate (range (count a-seq)))))))


;; Tail-recursive version; longer
(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [rotate (fn [[x & xs]]
                   (concat xs [x]))]
      (loop [n (count a-seq)
             a-seq a-seq
             rots []]
        (if (= n 0)
          rots
          (recur (dec n) (rotate a-seq) (conj rots a-seq)))))))

(comment
  ;; This version uses iterate; this is my favorite version
  (defn rotations [a-seq]
    (let [rotate (fn [[x & xs]]
                   (concat xs [x]))]
      (if (empty? a-seq)
        [[]]
        (take (count a-seq) (iterate rotate a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          value (inc (freqs k 0))
          new-freqs (assoc freqs k value)]
      (recur new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [[k v]] (repeat v k)) a-map))

(defn my-take [n coll]
  (loop [n n
         coll coll
         new-coll (empty coll)]
    (if (or (empty? coll) (= n 0))
      new-coll
      (recur (dec n) (rest coll) (conj new-coll (first coll))))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (recur (dec n) (rest coll))))

;; Returns vector of two list-like structures, not vectors
(defn halve-list [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    [(apply list (my-take midpoint a-seq)) (my-drop midpoint a-seq)]))

;; Returns vector of two vectors
(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    [(my-take midpoint a-seq) (vec (my-drop midpoint a-seq))]))

(comment
  ;; Helper function for tail-recursive version, expects lists
  (defn next-seqs-in-merge [a-seq b-seq m-seq]
    (let [a-val (first a-seq)
          b-val (first b-seq)]
      (if (< a-val b-val)
        [(rest a-seq) b-seq (conj m-seq a-val)]
        [a-seq (rest b-seq) (conj m-seq b-val)]))))

(comment
  ;; Tail-recursive, expects lists
  (defn seq-merge [a-seq b-seq]
    (loop [a-seq a-seq
           b-seq b-seq
           m-seq ()]
      (cond (empty? a-seq) (concat m-seq b-seq)
            (empty? b-seq) (concat m-seq a-seq)
            :else (let [next-seqs (next-seqs-in-merge a-seq b-seq m-seq)
                        [new-a-seq new-b-seq new-m-seq] next-seqs]
                    (recur new-a-seq new-b-seq new-m-seq))))))

;; Helper function for tail-recursive version, expects vectors
(defn next-seqs-in-merge [a-seq b-seq m-seq]
  (let [a-val (peek a-seq)
        b-val (peek b-seq)]
    (if (> a-val b-val)
      [(pop a-seq) b-seq (conj m-seq a-val)]
      [a-seq (pop b-seq) (conj m-seq b-val)])))

;; Tail-recursive, expects vectors
(defn seq-merge [a-seq b-seq]
  (loop [a-seq a-seq
         b-seq b-seq
         m-seq ()]
    (cond (empty? a-seq) (vec (concat b-seq m-seq))
          (empty? b-seq) (vec (concat a-seq m-seq))
          :else (let [next-seqs (next-seqs-in-merge a-seq b-seq m-seq)
                      [new-a-seq new-b-seq new-m-seq] next-seqs]
                  (recur new-a-seq new-b-seq new-m-seq)))))

(comment
  ;; Not tail-recursive, expects lists
  (defn seq-merge [a-seq b-seq]
    (let [a-val (first a-seq)
          b-val (first b-seq)]
      (cond (empty? a-seq) b-seq
            (empty? b-seq) a-seq
            (< a-val b-val) (conj (seq-merge (rest a-seq) b-seq) a-val)
            :else (conj (seq-merge a-seq (rest b-seq)) b-val)))))

;; Not tail-recursive, expects vectors
(defn seq-merge [a-seq b-seq]
  (let [a-val (peek a-seq)
        b-val (peek b-seq)]
    (cond (empty? a-seq) b-seq
          (empty? b-seq) a-seq
          (> a-val b-val) (conj (seq-merge (pop a-seq) b-seq) a-val)
          :else (conj (seq-merge a-seq (pop b-seq)) b-val))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[seq-1 seq-2] (halve a-seq)]
      (seq-merge (merge-sort seq-1) (merge-sort seq-2)))))

;; Helper function for "recur version" mono-chop
(defn mono-iter [mono-seq a-seq f]
  (if (empty? a-seq)
    [mono-seq []]
    (let [a-val (last mono-seq)
          b-val (first a-seq)]
      (if (not (f a-val b-val))
        [mono-seq a-seq]
        (recur (conj mono-seq b-val) (rest a-seq) f)))))

;; Tail-recursive version
(defn mono-chop [a-seq]
  (if (<= (count a-seq) 2)
    [a-seq []]
    (let [a-val (first a-seq)
          b-val (second a-seq)
          f (if (< a-val b-val)
              <=
              >=)]
      (mono-iter [a-val b-val] (rest (rest a-seq)) f))))

;; Helper function for "reduce version" mono-chop
(defn mono-step [f [mono-seq b-seq] next-item]
  (let [prev-item (last mono-seq)
        continue (and (empty? b-seq) (f prev-item next-item))]
    (cond continue [(conj mono-seq next-item) b-seq]
          :else [mono-seq (conj b-seq next-item)])))

;; Uses reduce instead of recur
;; Returns a two element vector
;; The first element is the monotonic sequence at the head of the
;; original sequence, the second element is the rest of the original
;; sequence without the beginning monotonic sequence
;; Returns [mono-seq rest-a-seq] such that
;; (= (concat mono-seq rest-a-seq) a-seq)
(defn mono-chop [a-seq]
  (if (<= (count a-seq) 2)
    [a-seq []]
    (let [a-val (first a-seq)
          b-val (second a-seq)
          f (if (< a-val b-val)
              <=
              >=)
          mono-step (partial mono-step f)]
      (reduce mono-step [[a-val b-val] []] (rest (rest a-seq))))))

(defn split-into-monotonics [a-seq]
  (loop [monos [] a-seq a-seq]
    (if (empty? a-seq)
      monos
      (let [[b-seq c-seq] (mono-chop a-seq)]
        (recur (conj monos b-seq) c-seq)))))

;; permutations helper function
;; Generates new permutations from sequence perm by inserting item at
;; each position in perm
(defn gen-perms [item perm]
  (let [num-positions (+ 1 (count perm))
        make-seq (fn [i]
                   (let [[start end] (split-at i perm)]
                     (concat start [item] end)))]
    (map make-seq (range num-positions))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (let [perms (permutations (rest a-set))
          gen-perms-1 (partial gen-perms (first a-set))]
      (mapcat gen-perms-1 perms))))

(defn gen-sets-with-e [e a-set]
  (set (map #(conj % e) a-set)))

;; Power set algorithm form Wikipedia
;; http://en.wikipedia.org/wiki/Power_set
(defn powerset [a-set]
  (let [a-set (set a-set)]
    (if (empty? a-set)
      #{#{}}
      (let [e (first a-set)
            T (disj a-set e)
            power-set-of-T (powerset T)
            power-set-of-T-with-e (gen-sets-with-e e power-set-of-T)]
        (clojure.set/union power-set-of-T power-set-of-T-with-e)))))
