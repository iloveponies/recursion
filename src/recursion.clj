(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn one-or-less? [coll]
  (or (empty? coll) (empty? (rest coll))))

(defn my-last [coll]
  (nth coll (dec (count coll)) nil))

(defn max-element [a-seq]
  (if (one-or-less? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (one-or-less? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (and
       (not (empty? a-seq))
       (pred? (first a-seq)))
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))
    '()))

(defn my-drop-while [pred? a-seq]
  (if (and
       (not (empty? a-seq))
       (pred? (first a-seq)))
    (my-drop-while pred? (rest a-seq))
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    ;; (= (first [nil]) (first [])) is of course true...
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))


(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    '()
    (cons
     (f (first a-seq) (first b-seq))
     (my-map f (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector '())
    (concat (vector a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  ;; Not quite sure about this..
  (map reverse (tails (reverse a-seq))))

(defn rotations-rec [prev a-seq]
  (if (empty? a-seq)
    '()
    (cons
     (concat a-seq prev)
     (rotations-rec (concat prev (vector (first a-seq))) (rest a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ;; How to get rid of this..
    '(())
    (rotations-rec '() a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [val (first a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
       (assoc freqs val (inc (get freqs val 0)))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [[key val] (first a-map)]
      (un-frequencies-helper
       (concat a-seq (repeat val key))
       (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (vector (my-take n a-seq) (my-drop n a-seq))))

;;; wat:
;; user> (conj (rest [2 3]) 4)
;; (4 3)
;; user> (conj [3] 4)
;; [3 4]

(defn insert-to-sorted [val seq]
  (let [first-val (first seq)
        second (first (rest seq))]
    (cond
      (> first-val val) (cons val seq)
      (not second) (concat seq (cons val '()))
      (<= first-val val second) (concat [first-val] [val] (rest seq))
      :else (cons first-val (insert-to-sorted val (rest seq))))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (seq-merge (rest a-seq) (insert-to-sorted (first a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (one-or-less? a-seq)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn inits-for-monotonics [a-seq]
  (rest (sort-by count (inits a-seq))))

(defn is-monotonic [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (one-or-less? a-seq)
    a-seq
    (let [monotonic-part (last (take-while is-monotonic (inits-for-monotonics a-seq)))]
      (concat (vector monotonic-part)
              (split-into-monotonics (drop (count monotonic-part) a-seq))))))

(defn powerset-adder [to-add a-set]
  (if (contains? a-set to-add)
    #{}
    (conj a-set to-add)))

(defn powerset-helper [res a-set]
  (if (empty? a-set)
    res
    (let [f (fn [x] (powerset-adder (first a-set) x))]
      (powerset-helper (set (clojure.set/union (map f res) res)) (rest a-set)))))

(defn powerset [a-set]
  (if (one-or-less? a-set)
    #{a-set}
    (powerset-helper (conj (set (map set (partition 1 a-set))) #{})
                     a-set)))

;; TODO: not done
(defn permutations [a-set]
  (let [a-seq (vector a-set)]
    (if (one-or-less? a-seq)
      a-seq
      nil)))

