(ns recursion)

(defn product-r [acc coll]
  (if (empty? coll)
    acc
    (recur (* (first coll) acc) (rest coll))))

(defn product [coll]
  (product-r 1 coll))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (empty? (rest coll)) (first coll)
    :else (recur (rest coll))))

(defn max-element-r [prev-max a-seq-rest]
  (if (empty? a-seq-rest)
    prev-max
    (recur (max prev-max (first a-seq-rest)) (rest a-seq-rest))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (max-element-r (first a-seq) (rest a-seq))))

(defn seq-max-r [seq-1-rest seq-2-rest seq-1 seq-2]
  (cond
    (empty? seq-1-rest) seq-2
    (empty? seq-2-rest) seq-1
    :else (recur (rest seq-1-rest) (rest seq-2-rest) seq-1 seq-2)))

(defn seq-max [seq-1 seq-2]
  (seq-max-r seq-1 seq-2 seq-1 seq-2))

(defn longest-seq-r [prev-max a-seq-rest]
  (if (empty? a-seq-rest) prev-max
    (recur (seq-max prev-max (first a-seq-rest)) (rest a-seq-rest))))

(defn longest-sequence [a-seq]
  (longest-seq-r nil a-seq))

(defn seq-or-empty [a-seq] (or (seq a-seq) `()))

(defn my-filter-r [seq-f pred? a-seq-rest]
  (let [a-first (first a-seq-rest)]
    (cond
      (empty? a-seq-rest) (seq-or-empty seq-f)
      (pred? a-first) (recur (conj (vec seq-f) a-first) pred? (rest a-seq-rest))
      :else (recur seq-f pred? (rest a-seq-rest)))))

(defn my-filter [pred? a-seq]
  (my-filter-r [] pred? a-seq))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (recur elem (rest a-seq))))

(defn my-take-while-r [seq-t pred? a-seq-rest]
  (let [a-first (first a-seq-rest)]
    (cond
      (empty? a-seq-rest) (seq-or-empty seq-t)
      (pred? a-first) (recur (conj (vec seq-t) a-first) pred? (rest a-seq-rest))
      :else (seq-or-empty seq-t))))

(defn my-take-while [pred? a-seq]
  (my-take-while-r [] pred? a-seq))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) `()
    (pred? (first a-seq)) (recur pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
    :else false))

(defn my-map-r [seq-r f seq-1-rest seq-2-rest]
  (if (or (empty? seq-1-rest) (empty? seq-2-rest))
    (seq-or-empty seq-r)
    (recur (conj
             seq-r
             (f (first seq-1-rest) (first seq-2-rest)))
           f
           (rest seq-1-rest)
           (rest seq-2-rest))))

(defn my-map [f seq-1 seq-2]
  (my-map-r [] f seq-1 seq-2))

(defn power-r [res n k-prev]
  (if (zero? k-prev)
    res
    (recur (* res n) n (dec k-prev))))

(defn power [n k]
  (power-r 1 n k))

(defn fib-r [fib-2 fib-1 count-down]
  (let [fib-n (+ fib-1 fib-2)]
    (if (zero? count-down)
      fib-n
      (recur fib-1 fib-n (dec count-down)))))

(defn fib [n]
  (cond
    (<= n 0) 0
    (<= n 2) 1
    :else (fib-r 1 1 (- n 3))))

(defn my-repeat-r [seq-r count-down what]
  (if (<= count-down 0)
    seq-r
    (recur (cons what seq-r) (dec count-down) what)))

(defn my-repeat [how-many-times what-to-repeat]
    (my-repeat-r `() how-many-times what-to-repeat))

(defn my-range-r [seq-r curr up-to]
  (let [next-curr (inc curr)]
    (if (>= curr up-to)
      seq-r
      (recur (cons curr seq-r) next-curr up-to))))

(defn my-range [up-to]
  (if (< up-to 0)
    `()
    (my-range-r `() 0 up-to)))

(defn tails-r [seq-r a-seq-rest]
  (if (empty? a-seq-rest)
    (cons a-seq-rest seq-r)
    (recur (cons a-seq-rest seq-r) (rest a-seq-rest))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (tails-r `() a-seq)))

(defn inits-r [seq-r seq-last a-seq-rest]
  (if (empty? a-seq-rest)
    seq-r
    (let [seq-next (conj seq-last (first a-seq-rest))]
      (recur (conj seq-r seq-next) seq-next (rest a-seq-rest)))))

(defn inits [a-seq]
  (inits-r [[]] [] a-seq))

(defn rotations-r [seq-r seq-last seq-rest]
  (if (empty? seq-rest)
    seq-r
    (let [seq-next (conj (subvec seq-last 1) (first seq-last))]
      (recur (cons seq-next seq-r) seq-next (rest seq-rest)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-r [] (vec a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-item (first a-seq)
          a-seq-next (next a-seq)
          item-freq (get freqs a-item 0)
          freqs-next (assoc freqs a-item (inc item-freq))]
      (recur freqs-next a-seq-next))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-recur [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [a-map-item (first a-map)
          a-map-next (next a-map)
          item (key a-map-item)
          item-count (val a-map-item)
          item-repeated (repeat item-count item)
          a-seq-next (concat a-seq item-repeated)]
      (recur a-seq-next a-map-next))))

(defn un-frequencies [a-map]
  (un-frequencies-recur `() a-map))

(defn my-split-recur [vec-before non-zero a-seq-after]
  (if (or (zero? non-zero) (empty? a-seq-after))
    (vector (or (seq vec-before) `()) a-seq-after)
    (let [a-item (first a-seq-after)
          a-seq-after-next (next a-seq-after)
          a-vec-before-next (conj vec-before a-item)]
      (recur a-vec-before-next (dec non-zero) a-seq-after-next))))

(defn my-take [n coll]
  (get (my-split-recur [] n coll) 0))

(defn my-drop-recur [non-zero a-seq]
  (if (or (zero? non-zero) (empty? a-seq))
    (or a-seq `())
    (recur (dec non-zero) (next a-seq))))

(defn my-drop [n coll]
  (my-drop-recur n coll))

(defn halve [a-seq]
  (let [a-count (count a-seq)
        middle (int (/ a-count 2))]
   (my-split-recur [] middle a-seq)))

(defn seq-merge-recur [a-vec-res a-seq b-seq]
  (let [a-item (first a-seq)
        b-item (first b-seq)]
    (cond
      (empty? a-seq) (concat a-vec-res b-seq)
      (empty? b-seq) (concat a-vec-res a-seq)
      (<= a-item b-item) (recur (conj a-vec-res a-item) (next a-seq) b-seq)
      :else (recur (conj a-vec-res b-item) a-seq (next b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-recur [] a-seq b-seq))

(defn merge-sort-recur [a-seq b-seq]
  (let [a-sorted? (>= 1 (count a-seq))
        b-sorted? (>= 1 (count b-seq))
        a-seq-sorted (if a-sorted?
                       a-seq
                       (apply merge-sort-recur (halve a-seq)))
        b-seq-sorted (if b-sorted?
                       b-seq
                       (apply merge-sort-recur (halve b-seq)))]
    (seq-merge a-seq-sorted b-seq-sorted)))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply merge-sort-recur (halve a-seq))))

(defn monotonic-predicate [a-item b-item]
  (if (>= a-item b-item) >= <=))

(defn item-monotonic? [m-seq item]
  (let [m-seq-first (first m-seq)
        m-seq-last (last m-seq)
        need-def? (== m-seq-first m-seq-last)
        m-seq-pred (monotonic-predicate m-seq-first m-seq-last)]
   (or need-def? (m-seq-pred m-seq-last item))))

(defn split-into-monotonics-recur [res-seq cur-seq rest-seq]
  (let [curr-item (first rest-seq)]
    (cond
      (empty? rest-seq) (conj res-seq cur-seq)
      (item-monotonic? cur-seq curr-item) (recur res-seq (conj cur-seq curr-item) (rest rest-seq))
      :else (recur (conj res-seq cur-seq) (vector curr-item) (rest rest-seq)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-recur [] [(first a-seq)] (next a-seq)))

(defn combine-set [a-set with-set]
  (map (fn [with-item] (concat with-item a-set)) with-set))

(defn split-set-recur [res-first res-next done-set rest-set]
  (if (empty? rest-set)
    (list res-first res-next)
    (recur
      (cons (first rest-set) res-first)
      (cons (concat done-set (rest rest-set)) res-next)
      (cons (first rest-set) done-set)
      (rest rest-set))))

(defn split-set [a-set]
  (split-set-recur () () () a-set))

(defn shuffle-two [a-set]
  (let [a-set-count (count a-set)]
    (cond 
      (>= 0 a-set-count) `()
      (== 1 a-set-count) a-set
      :else (let [a-set-first (first a-set)
                  a-set-second (second a-set)]
              (list (list a-set-first a-set-second) (list a-set-second a-set-first))))))

; Sample for [1 2 3 4 5]
; Init:
; - []
; - [[5] [4] [3] [2] [1]]
; - [[4 3 2 1] [5 3 2 1] [5 4 2 1] [5 4 3 1] [5 4 3 2]]
; Recur 1:
; - []
; - [[1 5] [2 5] [3 5] [4 5] [4] [3] [2] [1]]
; - [[2 3 4] [1 3 4] [1 2 4] [1 2 3] [5 3 2 1] [5 4 2 1] [5 4 3 1] [5 4 3 2]]
; Recur 2:
; - []
; - [[4 1 5] [3 1 5] [2 1 5] [2 5] [3 5] [4 5] [4] [3] [2] [1]]
; - [[3 2] [4 2] [4 3] [1 3 4] [1 2 4] [1 2 3] [5 3 2 1] [5 4 2 1] [5 4 3 1] [5 4 3 2]]
; Recur 3:
; - [[3 2 4 1 5] [2 3 4 1 5]]
; - [[3 1 5] [2 1 5] [2 5] [3 5] [4 5] [4] [3] [2] [1]]
; - [[4 2] [4 3] [1 3 4] [1 2 4] [1 2 3] [5 3 2 1] [5 4 2 1] [5 4 3 1] [5 4 3 2]]
(defn permutations-recur [res-set first-set next-set]
  (if (empty? next-set)
    res-set
    (let [a-set (first first-set)
          with-set (first next-set)
          splits (split-set with-set)
          firsts (map list (first splits))]
      (if (>= 2 (count with-set))
        (recur
          (concat (combine-set a-set (shuffle-two with-set)) res-set)
          (rest first-set)
          (rest next-set))
        (recur
          res-set
          (concat (combine-set a-set firsts) (rest first-set))
          (concat (second splits) (rest next-set)))))))

(defn permutations [a-set]
  (let [a-set-length (count a-set)]
    (cond
      (>= 0 a-set-length) (list ( list ))
      (== 1 a-set-length) (list a-set)
      (== 2 a-set-length) (shuffle-two a-set)
      :else (let [splits (split-set a-set)
                  firsts (map list (first splits))]
              (permutations-recur `() firsts (second splits))))))


(defn powerset [a-set]
  [:-])

