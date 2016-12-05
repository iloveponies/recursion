(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (apply * coll)))

(defn singleton? [coll]
  (and (not (empty? coll))(empty? (rest coll))))

(defn my-last [coll]
  (get coll (- (count coll) 1)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
  (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
       (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
       :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq)
        )))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)
    ))

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))(my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
    ))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
  (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
   (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() (seq a-seq))
  (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


(defn rotations [a-seq]
 (if (empty? a-seq)
    '(())
    (for [i (range (count a-seq))]
         (concat (drop i a-seq) (take i a-seq))
)))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq)
    freqs
    (contains? freqs (first a-seq))
    (my-frequencies-helper (assoc freqs (first a-seq)(inc (get freqs (first a-seq))))(rest a-seq))
    :else
    (my-frequencies-helper (assoc freqs (first a-seq) 1)(rest a-seq))
  ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map)
    '()
    :else
    (concat (repeat (get (first a-map) 1) (first(first a-map))) (un-frequencies (rest a-map)))
    ))

(defn my-take [n coll]
  (cond
    (== n 0)
    '()
    (empty? coll)
      '()
    :else
    (concat [(first coll)] (my-take (dec n) (rest coll) ))
    ))

(defn my-drop [n coll]
  (cond
  (> n (count coll))
    '()
  :else
  (reverse (my-take n (reverse coll)))
  ))

(defn halve [a-seq]
  (cond
    (== (count a-seq) 1)
    ['() '(1)]
  (odd? (count a-seq))
  (cons (my-take (int (/ (count a-seq) 2)) a-seq)[(my-take (- (count a-seq) (int (/ (count a-seq) 2)))(my-drop (+ (int (/ (count a-seq) 2)) 1)  a-seq))])
  :else
    (cons (my-take (int (/ (count a-seq) 2)) a-seq)[(my-take (- (count a-seq) (int (/ (count a-seq) 2)))(my-drop (int (/ (count a-seq) 2))  a-seq))])
    ))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    ))

(defn check-mon? [a-seq]
  (or (empty? a-seq)(apply <= a-seq)(apply >= a-seq)))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)
    '()
    (== 1 (count a-seq))
    a-seq
    :else
    (let [[b-seq c-seq] (halve a-seq)]
      (seq-merge (merge-sort b-seq) (merge-sort c-seq))
    )))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
      (let [longest (last (filter check-mon? (inits a-seq)))
        len (count longest)
        rema (drop len a-seq)]
    (cons longest (split-into-monotonics rema)))))

(defn permutations [a-set]
 (if (empty? a-set)
  (list ())
  (apply concat (map (fn [x] (map cons (repeat (first x)) (permutations (rest x)))) (rotations a-set)))
  ))

(defn powerset [a-set]
  [:-])

