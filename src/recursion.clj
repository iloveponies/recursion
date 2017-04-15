(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

;=> (* 1 (product (cons 2 (cons 4 '()))))
;;=> (* 1 (* 2 (product (cons 4 '()))))
;;;=> (* 1 (* 2 (* 4 (cons '()))))  ;empty, cons'() = 1(defn product [coll]
;;;;=> (* 1 (* 2 (* 4 (* 0))))
;;;;;=> (* 1 (* 2 (* 4 1)))
;;;;;;=> (* 1 (* 2 4))
;;;;;;;=> (* 1 8)
;;;;;;;;=> 8

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) ()
    (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq))) ())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (- k 1)))))

; iterative function to help with recursion
(defn fib-iter [a b n]
  (if (= n 0) b (fib-iter (+ a b) a (- n 1))))

(defn fib [n]
  (fib-iter 1 0 n))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (letfn [(helper [seq-1 seq-2]
            (if (empty? seq-1) '()
              (cons (concat seq-1 seq-2)(helper (rest seq-1) (concat seq-2 (take 1 seq-1))))))]
    (if (empty? a-seq) '(()) (helper a-seq '()))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (let [elem (first a-seq)
                elem-count (if (contains? freqs elem) (inc (get freqs elem)) 1)]
      (my-frequencies-helper (assoc freqs elem elem-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) '()
    :else (concat (repeat ((first a-map) 1)((first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (< n 1) '()
    (empty? coll) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (< n 1) (cons (first coll) (my-drop (dec n) (rest coll)))
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [a (int (/ (count a-seq) 2))]
    (vector (take a a-seq) (drop a a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))

(defn merge-sort [a-seq]
  (let [halves (halve a-seq)]
    (if (<= (count (halves 1)) 1) (seq-merge (halves 1) (halves 0))
      (seq-merge (merge-sort (halves 0)) (merge-sort (halves 1))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])




