(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

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
  (if (>= (count seq-2) (count seq-1))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (not (pred? (first a-seq))) (my-filter pred? (rest a-seq))
    :else (cons (first a-seq)
                (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) ()
    :else (cons what-to-repeat (my-repeat (dec how-many-times)
                                          what-to-repeat))))

(defn my-range [up-to]
  (cond
    (zero? up-to) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (defn inits-helper [a-seq]
    (cond
      (empty? a-seq) '(())
      :else (cons (seq a-seq) (inits-helper
                          (reverse (rest (reverse a-seq)))))))
  (reverse (inits-helper a-seq)))

(defn rotations [a-seq]
  (defn acc-rots [acc a-seq k]
    (cond
      (zero? k) acc
      :else (let [rot (concat (rest a-seq) [(first a-seq)])]
              (acc-rots (conj acc rot) rot (dec k)))))
  (cond
    (empty? a-seq) [[]]
    :else (acc-rots [a-seq] a-seq (dec (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (let [el-count (fn [el]
                           (if (contains? freqs el)
                             (freqs el)
                             0))
                elem (first a-seq)
                new-elem-count (inc (el-count elem))]
            (my-frequencies-helper
              (assoc freqs elem new-elem-count)
              (rest a-seq)))))

(defn my-frequencies [a-seq]
  (cond
    (empty? a-seq) {}
    :else (my-frequencies-helper {} a-seq)))

(defn un-frequencies [a-map]
  (defn un-frequencies-helper [a-seq a-map]
    (cond
      (empty? a-map) a-seq
      :else (let [n (second (first a-map))
                  elem (first (first a-map))]
              (un-frequencies-helper
                (concat a-seq (repeat n elem))
                (rest a-map)))))
  (cond
    (empty? a-map) ()
    :else (un-frequencies-helper () a-map)))

(defn my-take [n coll]
  (defn my-take-helper [acc n coll]
    (cond
      (or (zero? n) (empty? coll)) acc
      :else (my-take-helper
              (conj acc (first coll))
              (dec n)
              (rest coll))))
  (my-take-helper [] n coll))

(defn my-drop [n coll]
  (cond
    (or (< n 1) (empty? coll)) coll
    :else (my-drop
            (dec n)
            (rest coll))))

(defn halve [a-seq]
  (cond
    (empty? a-seq) (vector () ())
    :else (let [total-len (count a-seq)
                len1 (int (/ total-len 2))]
            (vector (my-take len1 a-seq)
                    (my-drop len1 a-seq)))))

(defn seq-merge [a-seq b-seq]
  (defn seq-merge-helper [acc a-seq b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq)) acc
      (empty? a-seq) (concat acc b-seq)
      (empty? b-seq) (concat acc a-seq)
      :else (if (<= (first a-seq) (first b-seq))
              (seq-merge-helper (conj acc (first a-seq))
                                (rest a-seq)
                                b-seq)
              (seq-merge-helper (conj acc (first b-seq))
                                a-seq
                                (rest b-seq)))))
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (seq-merge-helper [] a-seq b-seq)))

(defn merge-sort [a-seq]
  (cond
    (or (empty? a-seq) (singleton? a-seq)) a-seq
    :else (let [[seq1 seq2] (halve a-seq)]
            (seq-merge (merge-sort seq1)
                       (merge-sort seq2)))))

(defn split-into-monotonics [a-seq]
  ;; given a sequence of numbers, the first
  ;; two will normally establish an initial
  ;; monotonic trend. If the sequence starts
  ;; with repeating element values, then the
  ;; first element after the repeating elements
  ;; will determine the initial trend.
  ;; A sequence of all elements of the same value
  ;; is arbitrarily set as monotonically increasing.
  (defn initial-trend-predicate [a-seq]
    (cond
      (empty? a-seq) <=
      (singleton? a-seq) <=
      :else (loop [n (count a-seq)
                   k 2
                   inits-seq (take k a-seq)
                   func <=]
              (if (< n k)
                func
                (if (not (apply = inits-seq))
                  (if (apply <= inits-seq)
                    <=
                    >=)
                  (recur
                    n
                    (inc k)
                    (take (inc k) a-seq)
                    func))))))

  (defn monotonic-predicate? [pred? a-seq]
    (apply pred? a-seq))

  ;; checks to see if the second element in the
  ;; inits sequence will break the trend, implying
  ;; that the first element is the largest for the
  ;; current trend
  (defn trend-change-ahead? [current-pred? inits-seq]
    (cond
      (or (empty? inits-seq) (singleton? inits-seq)) false
      (not (monotonic-predicate? 
             current-pred?
             (first (rest inits-seq)))) true
      :else false))

  ;; main body of split-into-monotonics
  (if (empty? a-seq)
    '(())
    (let [let-seq (drop 1 (inits a-seq))]
      (if (singleton? let-seq)
        let-seq
        (loop [pred? (initial-trend-predicate a-seq)
               loop-seq a-seq
               inits-seq let-seq
               acc []
               n (count (first inits-seq))]
          (if (singleton? inits-seq)
            (conj acc (first inits-seq))
            (recur
              (if (trend-change-ahead? pred? inits-seq)
                (initial-trend-predicate (drop n loop-seq))
                pred?)
              (if (trend-change-ahead? pred? inits-seq)
                (drop n loop-seq)
                loop-seq)
              (if (trend-change-ahead? pred? inits-seq)
                (drop 1 (inits (drop n loop-seq)))
                (rest inits-seq))
              (if (trend-change-ahead? pred? inits-seq)
                (conj acc (first inits-seq))
                acc)
              (if (trend-change-ahead? pred? inits-seq)
                (count (first (rest (drop 1 (inits (drop n loop-seq))))))
                (count (first (rest inits-seq)))))))))))

(defn permutations [a-set]
  (defn add-permutated-tail [elem permutated-tail]
    (loop [acc []
           p-tail permutated-tail]
      (if (empty? p-tail)
        acc
        (recur
          (cons (cons elem (first p-tail)) acc)
          (rest p-tail)))))
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (conj () (seq a-set))
    :else (loop [loop-set a-set
                 acc []
                 elem (first loop-set)]
            (if (empty? loop-set)
              acc
              (recur
                (rest loop-set)
                (concat (add-permutated-tail
                        elem
                        (permutations (disj (set a-set) elem))) acc)
                (first (rest loop-set)))))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (loop [loop-set (set a-set)
                 acc (conj #{} (set a-set))]
            (if (empty? loop-set)
              (conj acc #{})
              (recur
                (rest loop-set)
                (clojure.set/union (conj acc
                            (conj #{} (first loop-set)))
                      (powerset (disj (set a-set) (first loop-set)))))))))

