(ns recursion
  (:require [clojure.set :as set]))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [seqs]
  (cond
    (empty? seqs) nil
    (singleton? seqs) (first seqs)
    :else (seq-max (first seqs) (longest-sequence (rest seqs)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          filtered-rst (my-filter pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst filtered-rst)
        filtered-rst))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [fst (first a-seq)]
      (if (pred? fst)
        (cons fst (my-take-while pred? (rest a-seq)))
        '()
        ))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (let [check-equality (fn chk [as bs]
                         (cond
                           (and (empty? as) (empty? bs)) true
                           (= (first as) (first bs)) (chk (rest as) (rest bs))
                           :else false))]
    (if (= (count a-seq) (count b-seq))
      (check-equality a-seq b-seq)
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (*' n (power n (dec k)))))

; optimized with memoize macro, but still not a tail recursion
; explore loop...recur construct
; +' promoting to bigint
(def fib
  (memoize (fn [n] (if (< n 2)
                     n
                     (+' (fib (dec n)) (fib (- n 2)))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [decreased (dec up-to)]
    (if (< up-to 1)
      '()
      (cons decreased (my-range decreased)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reverted-tails (tails (reverse a-seq))]
    (map reverse (reverse reverted-tails))))

(defn rotations [a-seq]
  (let [tls (tails a-seq)
        ins (inits a-seq)]
    (seq (set (concat (map concat tls ins))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-elem (first a-seq)
          count-for-current (inc (get freqs current-elem 0))]
      (my-frequencies-helper
        (assoc freqs current-elem count-for-current)
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [freqs]
  (if (empty? freqs)
    '()
    (let [[elem amount] (first freqs)]
      (concat (repeat amount elem) (un-frequencies (rest freqs))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[half-1 half-2] (halve a-seq)]
      (seq-merge (merge-sort half-1) (merge-sort half-2)))))

(defn split-into-monotonics [a-seq]
  (let [inits' (inits a-seq)
        monotonous-range (my-last (my-take-while
                                    (fn [x] (if (empty? x)
                                              true
                                              (or (apply <= x) (apply >= x))))
                                    inits'))]
    (if (empty? a-seq)
      '()
      (cons monotonous-range (split-into-monotonics (my-drop (count monotonous-range) a-seq))))))

; documenting my impl for myself:
; tests were barking at (perms []) case due to the type of collection
; 1234 -> rots: 1234 2341 3412 4123
; so we take 1st rot, take its head 1 and append rots of the rest. do it recursively (after finished we take 2nd rot, take its head 2 ...)
;  234 -> rots: 234 342 423
;   34 -> rots: 34 43
;    4 -> rots: 4
; P(n) = n!
(defn permutations [a-set]
  (let [perms' (fn perms [prefix a-set]
                 (cond
                   (empty? a-set) (vector prefix)
                   :else (let [rots (rotations a-set)]
                           (mapcat identity ; one-level seq flattening
                             (map
                               (fn [rot]
                                 (let [head-elem (first rot)]
                                   (perms (cons head-elem prefix) (rest rot))))
                               rots)))))]
    (perms' [] a-set)))

; documenting my impl for myself:
; 124
; 1
; .
;  24
;  2
;  .
;   4
;   .
(defn powerset [a-set]
  (if (empty? a-set)
      #{#{}}
    (let [as-seq (seq a-set)]
      (for [curr-ps #{#{} #{(first as-seq)}}
            tail-ps (map #(set/union curr-ps %) (powerset (set (rest as-seq))))]
        tail-ps))))