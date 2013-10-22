(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; (product [1 2 4])
; (* 1 (product [2 4]))
; (* 1 (* 2 (product [4])))
; (* 1 (* 2 (* 4 (product []))))
; (* 1 (* 2 (* 4 1)))
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (if (empty? coll)
    false
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
    :else (max-element (cons (max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(defn longer [seq-1 seq-2]
  (cond 
    (and (empty? seq-1) (empty? seq-2)) :neither
    (empty? seq-1) :second
    (empty? seq-2) :first
    :else (longer (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (case (longer seq-1 seq-2)
    :first seq-1
    :second seq-2
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (my-filter pred? (rest a-seq))]
      (if (pred? f)
        (cons f r)
        r))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (case n
    0 0
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (inits (butlast a-seq)))))

(defn rotations-helper [seq-1 seq-2]
  (if (empty? seq-2)
    nil
    (cons (concat seq-1 seq-2) (rotations-helper (cons (last seq-2) seq-1) (butlast seq-2)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper () a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-count (+ 1 (or (get freqs elem) 0))]
      (my-frequencies-helper (assoc freqs elem new-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))
  
(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq))
        ()
      (or (empty? b-seq) (and (not (empty? a-seq)) (< a b)))
        (cons a (seq-merge (rest a-seq) b-seq))
      :else
        (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[f s] (halve a-seq)]
      (seq-merge (merge-sort f) (merge-sort s)))))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [s (first (filter monotonic? (inits a-seq)))]
      (cons s (split-into-monotonics (drop (count s) a-seq))))))

(defn permutations-helper [cur a-set]
  (if (empty? a-set)
    (seq [cur])
    (apply concat (for [e a-set]
      (permutations-helper (cons e cur) (disj a-set e))))))

(defn permutations [a-set]
  (permutations-helper () (set a-set)))

(defn flatten-tree [a-set]
  (clojure.set/union (first a-set) (second a-set)))

(defn powerset-helper [sets a-set]
  (if (empty? a-set)
    (cons sets nil)
    (clojure.set/union
      (powerset-helper sets (rest a-set))
      (powerset-helper (conj sets (first a-set)) (rest a-set)))))

(defn powerset [a-set]
  (powerset-helper #{} a-set))

