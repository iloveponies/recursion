(ns recursion)

(defn product
  [coll]
  (if (empty? coll)
    1                ;; first branch always the base case, value when empty
    (* (first coll)
       (product (rest coll)))))  ;; second branch determines what operation to apply on the elements of the coll


;;; (product '(1 2 4))
;;; = (* 1 (product (cons 2 (cons 4 '()))))   linear - (grows linearly with input size)
;;;   (* 1 (* 2 (product (cons 4 '()))))
;;;   (* 1 (* 2 (* 4 (product '()))))
;;;   (* 1 (* 2 (* 4 1)))
;;;   (* 1 (* 2 4))
;;;   (* 1 8)
;;;   8

(defn singleton?
  [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last
  [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element
  [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max
  [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence
  [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter
  [pred? a-seq]
  (let [pred-fn (fn [x]
                  (if (pred? x)
                    x false))]
    (cond
      (empty? a-seq) a-seq
      (false? (pred-fn (first a-seq))) (my-filter pred? (rest a-seq))
      :else (cons (pred-fn (first a-seq))
                  (my-filter pred? (rest a-seq))))))

(defn sequence-contains?
  [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while
  [pred? a-seq]
  (let [pred-fn (fn [x]
                  (if (pred? x)
                    x false))]
    (cond
      (empty? a-seq) a-seq
      (false? (pred-fn (first a-seq))) '()
      :else (cons (pred-fn (first a-seq))
                  (my-take-while pred? (rest a-seq))))))

(defn my-drop-while
  [pred? a-seq]
  (let [pred-fn (fn [x]
                  (if (not (pred? x))
                    x false))]
    (cond
      (empty? a-seq) a-seq
      (false? (pred-fn (first a-seq))) (my-drop-while pred? (rest a-seq))
      :else (cons (first a-seq)
                  (my-drop-while true? (rest a-seq))))))

(defn seq=
  [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map
  [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power
  [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib
  [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat
  [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range
  [up-to]
  (if (= 0 up-to)
    '()
    (cons (- up-to 1)
          (my-range (dec up-to)))))

(defn tails
  [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits
  [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq)
          (inits (reverse (rest (reverse a-seq)))))))

(defn rotations
  [a-seq]
  (if (empty? a-seq) (cons '() '())
      (distinct (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper
  [freqs a-seq]
  (let [frst (first a-seq)
        rst (rest a-seq)]
    (cond
      (empty? a-seq) freqs
      (get freqs frst) (my-frequencies-helper
                        (assoc freqs frst (inc (get freqs frst)))
                        rst)
      :else (my-frequencies-helper
             (assoc freqs frst 1)
             rst))))

(defn my-frequencies
  [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies
  [a-map]
  (let [frst-vec (first a-map)]
    (if (empty? a-map)
      '()
      (concat
       (repeat (second frst-vec) (first frst-vec))
       (un-frequencies (dissoc a-map (first frst-vec)))))))

(defn my-take
  [n coll]
  (cond
    (empty? coll) '()
    (not (= 0 n)) (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop
  [n coll]
  (cond
    (empty? coll) '()
    (not (= 0 n)) (my-drop (dec n) (rest coll))
    :else coll))

(defn halve
  [a-seq]
  (let [half (-> a-seq count (/ 2) int)]
    (cond
      (empty? a-seq) '()
      (= 1 (count a-seq)) [() a-seq]
      :else [(my-take half a-seq) (my-drop half a-seq)])))

(defn seq-helper
  [a-seq b-seq]
  (if (< (apply min a-seq) (apply min b-seq))
    [(rest a-seq) b-seq]
    [a-seq (rest b-seq)]))

(defn seq-merge
  [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))'()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (cons (min (apply min a-seq) (apply min b-seq))
                (seq-merge (first (seq-helper a-seq b-seq)) (second (seq-helper a-seq b-seq))))))

(defn merge-sort
  [a-seq]
  (if (or (= 1 (count a-seq)) (empty? a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
