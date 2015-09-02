(ns recursion)

(defn product [coll]
  (if (empty? coll)
   1
   (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (max (first a-seq)
           (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= (first a-seq) elem) true
    :else                  (sequence-contains? elem
                                               (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (empty? a-seq) (empty? b-seq)))
      false
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else   (+ (fib (- n 1))
               (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

; [] [1 2 3] -> [1 2 3] ++ [] -> [1 2 3]
; [1] [2 3]  -> [1] ++ [2 3]  -> [2 3 1]
; [2 1] [3]  -> [3] ++ [1 2]  -> [3 1 2]
(defn rotations-helper [used a-seq]
  (if (empty? a-seq)
    nil
    (cons (concat a-seq (reverse used))
          (rotations-helper (cons (first a-seq)
                                  used)
                            (rest a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (rotations-helper [] a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [key (first a-seq)]
    (cond
      (empty? a-seq)
        freqs
      (contains? freqs key)
        (my-frequencies-helper (assoc freqs key (inc (freqs key)))
                               (rest a-seq))
      :else
        (my-frequencies-helper (assoc freqs key 1)
                               (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[key value] (first a-map)]
    (if (empty? a-map)
      nil
      (concat (repeat value key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (zero? n))
    nil
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (zero? n)     coll
    :else         (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(take mid a-seq)
     (drop mid a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq)     a-seq
   (singleton? a-seq) a-seq
   :else
    (let [[fst-half snd-half] (halve a-seq)]
      (seq-merge (merge-sort fst-half)
                 (merge-sort snd-half)))))


(defn monotonic? [xs]
 (or (apply < xs)
     (apply > xs)))

(defn monotonics-helper [sequence monotonics]
  (let [candidate (cons (first sequence)
                        (first monotonics))]
    (cond
      (empty? sequence) monotonics
      (monotonic? candidate)
        (monotonics-helper (rest sequence)
                           (cons candidate
                                 (rest monotonics)))
      :else
        (monotonics-helper (rest sequence)
                           (cons (list (first sequence)) monotonics)))))

(defn split-into-monotonics [a-seq]
  (reverse (map reverse (monotonics-helper a-seq (list '())))))


(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat
           (map (fn [x] (rotations x))
                (map (fn [x] (conj x (first a-set)))
                     (permutations (rest a-set)))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [pset (powerset (rest a-set))]
      (concat pset
              (map (fn [s]
                     (conj s (first a-set)))
                   pset)))))

