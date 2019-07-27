(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll)
                         (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (recur (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
      (singleton? a-seq) (first a-seq)
      :else (max (first a-seq)
                 (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq)
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
      (let [element (first a-seq)]
        (if (pred? element) (cons element (my-filter pred? (rest a-seq)))
            (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
      (let [elem (first a-seq)]
        (if (pred? elem) (cons elem (my-take-while pred? (rest a-seq)))
            '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
      (let [elem (first a-seq)]
        (if (pred? elem) (my-drop-while pred? (rest a-seq))
                a-seq))))

(defn seq= [& seqs]
  (cond (every? empty? seqs) true
        (some empty? seqs) false
        :else (and (apply = (map first seqs))
           (apply seq= (map rest seqs)))))

(defn my-map [f seq-1 seq-2]
  (if (some empty? [seq-1 seq-2]) '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

;; the fun version--but requires bootstrapping with a version of `map` that takes
;; one sequence
(defn my-map [f & seqs]
  (if (some empty? seqs) '()
      (cons (apply f (map first seqs))
            (apply my-map (cons  f (map rest seqs))))))

(defn square [n] (* n n))
(defn power [n k]
  (cond (zero? k) 1
        (even? k) (square (power n (/ k 2)))
        :else (* n (power n (- k 1)))))

(defn fib [n]
  (defn fib-helper [a b n]
    (if (= n 0) b
        (recur b (+ a b) (- n 1))))
  (fib-helper 1 0 n))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) '()
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to) '()
      (let [next (- up-to 1)]
        (cons next (my-range next)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (defn rotations-helper [n a-seq acc]
    (if (= n (count a-seq)) acc
        (recur (inc n)
               (concat (rest a-seq) [(first a-seq)])
               (cons a-seq acc))))
  (if-not (zero? (count a-seq))  (rotations-helper 0 a-seq '())
          `(~a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
      (let [elem (first a-seq)]
        (recur (update-in freqs [elem] (fnil inc 0))
               (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
      (let [[elem count] (first a-map)]
        (concat (repeat count elem)
                (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll)) '()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-length (quot (count a-seq) 2)]
    [(take half-length a-seq)
     (drop half-length a-seq)]))

(defn seq-merge [a-seq b-seq]
  (defn seq-merge-helper [result a-seq b-seq]
    (cond (empty? a-seq) (concat result b-seq)
          (empty? b-seq) (concat result a-seq)
          :else (let [a (first a-seq)
                      b (first b-seq)]
                  (if (< a b)
                    (seq-merge-helper (concat result [a]) (rest a-seq) b-seq)
                    (seq-merge-helper (concat result [b]) a-seq (rest b-seq))))))
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) a-seq
      (->> a-seq halve (map merge-sort) (apply seq-merge))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) '()
        (= (count a-seq) 1) (list a-seq)
        :else (let [piece (last (take-while monotonic? (drop 2 (inits a-seq))))
                    length (count piece)]
                (cons piece
                      (split-into-monotonics (drop length a-seq))))))

(defn indexed [s] ;; from contrib
  (map vector (iterate inc 0) s))

(defn without [index a-seq]
  (concat (take index a-seq)
          (drop (inc index) a-seq)))

(defn permutations [a-set]
  (cond (zero? (count a-set)) '(())
        (= 1 (count a-set)) (list (seq a-set))
        :else (apply concat
                     (for [[n element] (indexed a-set)]
                       (map (partial cons element)
                            (permutations (without n a-set)))))))

(defn powerset [a-set]
  (cond (empty? a-set) (set [#{}])
        :else (let [smaller (powerset (rest a-set))]
                (->> smaller
                     (map (partial cons (first a-set)))
                     (concat smaller)
                     (map set)))))

;; slow
;; (defn powerset [a-set]
;;   (->> a-set
;;        permutations
;;        (map inits)
;;        (apply concat)
;;        (map set)
;;        set))
