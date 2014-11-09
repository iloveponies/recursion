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
  (if (not (empty? coll))
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (not (empty? a-seq))
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (not (empty? a-seq))
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [my-filter-rec (my-filter pred? (rest a-seq))
          first-seq (first a-seq)]
      (if (pred? first-seq)
        (cons first-seq my-filter-rec)
        my-filter-rec))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-seq (first a-seq)]
    (if (pred? first-seq)
      (cons first-seq (my-take-while pred? (rest a-seq)))
      '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or  (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (cond
    (or  (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [up-to-dec (dec up-to)]
    (if (< up-to-dec 0)
      '()
      (cons up-to-dec (my-range up-to-dec)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [add-rot (fn add-rotf [part1 part2] (if (empty? part2)
                                      '()
                                      (cons (concat part2 part1) (add-rotf (conj part1 (first part2)) (rest part2)))))]
      (add-rot [] a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [freq-first (get freqs (first a-seq))]
      (my-frequencies-helper
        (assoc freqs (first a-seq)
          (if freq-first (inc freq-first) 1))
        (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[key val] (first a-map)]
    (concat (repeat val key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [num (int (/ (count a-seq) 2))]
    (cons (my-take num a-seq) (cons (my-drop num a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (let [push-elem-a (fn [f-recursive] (cons (first a-seq) (f-recursive (rest a-seq) b-seq)))
        push-elem-b (fn [f-recursive] (cons (first b-seq) (f-recursive a-seq (rest b-seq))))]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (and (not (empty? a-seq)) (not (empty? b-seq))) (if (< (first a-seq) (first b-seq))
                                                      (push-elem-a seq-merge)
                                                      (push-elem-b seq-merge))
    (empty? a-seq) (push-elem-b seq-merge)
    (empty? b-seq) (push-elem-a seq-merge))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [seq-diff (fn [b-seq] (map - b-seq (rest b-seq)))
          seq-pend-change (fn [c-seq] (map * (seq-diff c-seq) (rest (seq-diff c-seq))))
          number-monotonic (fn [d-seq] (+ 2 (count (take-while pos? (seq-pend-change d-seq)))))]
      (cons (take (number-monotonic a-seq) a-seq) (split-into-monotonics (drop (number-monotonic a-seq) a-seq)) ))))

(defn my-insert [elem position a-seq]
    (concat (take position a-seq) (cons elem (drop position a-seq))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [derive-perm (fn [elem a-seq] (map (fn [position] (my-insert elem position a-seq)) (range 0 (inc (count a-seq)))))]
      (apply concat (map (fn [b-seq] (derive-perm (first a-set) b-seq)) (permutations (rest a-set)))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [get-from-set (fn getff [a-set position] (if (= 0 position)
                                                    (first a-set)
                                                    (getff (rest a-set) (dec position))))
          remove-pos-set (fn [position b-set] (disj (set b-set) (get-from-set b-set position)) )]
      (set (cons (set a-set) (apply concat        (map (fn [position] (powerset (remove-pos-set position a-set))) (range 0 (count a-set)))       ))))))
