(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; Write down the evaluation of: (product [1 2 4])
; (product (cons 1 (cons 2 (cons 4 '()))))
;=> (* 1 (product (cons 2 (cons 4 '()))))
;=> (* 1 (* 2 (product (cons 4 '()))))
;=> (* 1 (* 2 (* 4 (product '()))))
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

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
    (empty? a-seq)                  (if (empty? b-seq) true false)
    (empty? b-seq)                  false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else                           false))

(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    '()
    (cons (f (first a-seq) (first b-seq))
          (my-map f (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (== n 1)  1
    :else     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
      (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0) '() (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(()) (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))) ; dirty hack

(defn shift [a-seq n]
  (concat (drop n a-seq) (take n a-seq)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [n] (shift a-seq n)) (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          freq (if (contains? freqs elem)
                 (inc (get freqs elem))
                 1)]
      (my-frequencies-helper (assoc freqs elem freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[elem freq] (first a-map)]
      (concat (repeat freq elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [m (int (/ (count a-seq) 2))]
    [(take m a-seq) (drop m a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else          (let [a (first a-seq)
                         b (first b-seq)]
                     (if (<= a b)
                       (cons a (seq-merge (rest a-seq) b-seq))
                       (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[seq1 seq2] (halve a-seq)]
      (seq-merge (merge-sort seq1) (merge-sort seq2)))))

(defn monotonic? [a-seq]
  (or (empty? a-seq) (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic (last (take-while monotonic? (inits a-seq)))]
      (cons monotonic
            (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-seq]
  (let [a-set (set a-seq)]
    (if (empty? a-set)
      '(())
      (apply concat (map (fn [x] (map (fn [ps] (cons x ps))
                                      (permutations (disj a-set x))))
                         a-set)))))

(defn powerset-helper [acc a-set]
  (if (empty? a-set)
    acc
    (let [my-conj (fn [my-set] (conj my-set (first a-set)))
          new-set (clojure.set/union acc (map my-conj acc))]
      (powerset-helper new-set (rest a-set)))))

(defn powerset [a-set]
  (powerset-helper #{#{}} a-set))
