(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

;; (defn my-last [coll]
;;   (if (empty? (rest coll))
;;     (first coll)
;;     (my-last (rest coll))
;;     ))

;; (defn my-last [coll]
;;   (let [r (rest coll)]
;;     (if (seq r)
;;       (my-last (rest coll))
;;       (first coll)
;;       )))

(defn my-last [coll]
  (let [[x & xs] coll]
    (if xs
      (my-last xs)
      x)))

(defn max-element [a-seq]
  (let [[x & xs] a-seq]
    (if xs
      (max x (max-element xs))
      x)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [[x & xs] a-seq]
    (if xs
      (seq-max x (longest-sequence xs))
      x)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq)         false
        (= (first a-seq) elem) true
        :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq)        '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq)        '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or  (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
        :else                               false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond (== n 0) 0
        (== n 1) 1
        :else    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (my-map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [e (first a-seq)]
      (recur (assoc freqs e (inc (get freqs e 0))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [[e cnt]] (repeat cnt e)) a-map))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))
    ))

(defn my-drop [n coll]
  (cond (empty? coll) '()
        (<= n 0)      coll
        :else         (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq)                  b-seq
        (empty? b-seq)                  a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else                           (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [n (count a-seq)]
    (cond (<= n 1) a-seq
          :else    (let [[seq1 seq2] (halve a-seq)]
                     (seq-merge (merge-sort seq1) (merge-sort seq2))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [inc-seq (take-while (fn [[a b]] (<= a b)) (map vector a-seq (rest a-seq)))
          dec-seq (take-while (fn [[a b]] (>= a b)) (map vector a-seq (rest a-seq)))
          n       (+ 1 (max (count inc-seq) (count dec-seq)))]
      (cons (take n a-seq) (split-into-monotonics (drop n a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (->> (rotations a-set)
         (mapcat (fn [[x & xs]] (map (fn [ys] (cons x ys)) (permutations xs))))
         )))

(defn powerset [a-set]
  (letfn [(helper [a-set]
            (if (empty? a-set)
              #{#{}}
              (conj (set (mapcat #(powerset (disj a-set %)) a-set))
                    a-set)))]
    (helper (set a-set))))
