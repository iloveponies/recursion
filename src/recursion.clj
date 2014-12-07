(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
      (* (first coll)
         (product (rest coll)))))

(defn singleton? [coll]
  (and (coll? coll) (= (count coll) 1)))


(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
      (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (if (> (count (first a-seq)) (count (first (rest a-seq))))
                (longest-sequence (cons (first a-seq) (rest (rest a-seq))))
                (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))
    
(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while
  "Returns a sequence of the items in coll starting from the first
  item for which (pred item) returns logical false."
  [pred? a-seq]
  (cond (empty? a-seq) '()
        (not (pred? (first a-seq))) a-seq
        :else (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
  (if (not (= (count a-seq) (count b-seq))) false
      (cond
       (and (empty? a-seq) (empty? b-seq)) true
       (not (= (first a-seq) (first b-seq))) false
       :else (seq= (rest a-seq) (rest b-seq)))))

(defn my-map
  "Write the function (my-map f seq-1 seq-2) that returns a sequence of
  the following kind . The first item is the return value of f called
  with the first values of seq-1 and seq-2. The second item is the
  return value of f called with the second values of seq-1 and seq-2
  and so forth until seq-1 or seq-2 ends.

  This is actually exactly how map works when given two sequences, but
  for the sake of practice don't use map when defining my-map."
  [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond (zero? k) 1
        (= 1 k) n
        :else (* n (power n (dec k)))))

(defn fib
  "Compute the nth Fibonacci number. The nth Fibonacci number, Fn, is defined as:
     F0=0
     F1=1
     Fn=Fn-1+Fn-2"
  [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2))
        )))


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '()
      (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq) [()]
        (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) [()]
      (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (seq a-seq)
    (map
     (fn [n _]
       (concat (drop n a-seq) (take n a-seq)))
     (iterate inc 0) a-seq) ;; map stops at the smaller coll
    '(())))
      
;;GJG aaannnd we get around to shleping state with helper functions
(defn my-frequencies-helper
  [freqs [first-a & rest-a :as a-seq]]
  (if (empty? a-seq) freqs
      (let [new-freq
            (if (contains? freqs first-a)
              (assoc freqs first-a (inc (get freqs first-a)))
              (assoc freqs first-a 1))]
        (my-frequencies-helper new-freq rest-a))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (flatten
   (map #(repeat (val %) (key %)) a-map)))


(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll)
     (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [halfway (int (/ (count a-seq) 2))]
    [(my-take halfway a-seq) (my-drop halfway a-seq)]
    ))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond (empty? (rest a-seq)) a-seq
        :else
          (apply seq-merge (map merge-sort (halve a-seq)))))

;; GJG a version of merge sort that doesn't blow the stack 
;; (defn merge-seqs
;;   "Merges two sorted sequences into a single sorted sequence"
;;   ([left right]
;;     (merge-seqs (list left right)))
;;   ([[left right]]
;;     (loop [l left, r right, result []]
;;       (let [lhead (first l), rhead (first r)]
;;         (cond
;;           (nil? lhead)     (concat result r)
;;           (nil? rhead)     (concat result l)
;;           (<= lhead rhead) (recur (rest l) r (conj result lhead))
;;           true             (recur l (rest r) (conj result rhead)))))))

;; (defn mergesort
;;   "Produces a sorted sequence from an input sequence.
;;   Works best with vectors (since it uses 'count' internally)."
;;   [xs]
;;   ((fn mergesort-counted [xs n]
;;      (if (<= n 1)
;;        xs
;;        (let [middle (bit-shift-right n 1)]  ; fast division by 2
;;          (merge-seqs (map mergesort-counted 
;;                           (split-at middle xs)        ; two halves
;;                           [middle (- n middle)])))))  ; count of each half
;;    xs (count xs)))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

