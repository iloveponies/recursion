(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

;   (product [1 2 4])
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll))
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
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

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
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq n]
  (let [rotated-seq (concat (rest a-seq) (cons (first a-seq) '()))]
    (if (< n 2)
      (cons rotated-seq '())
      (cons rotated-seq (rotations-helper rotated-seq (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-count (if (contains? freqs elem) (inc (get freqs elem)) 1)]
      (my-frequencies-helper (assoc freqs elem elem-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (my-repeat ((first a-map) 1) ((first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (< n 1) (cons (first coll) (my-drop (dec n) (rest coll)))
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [first-n (int (/ (count a-seq) 2))]
    (vector (my-take first-n a-seq)
            (my-drop first-n a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   (< (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [halves (halve a-seq)]
    (if (<= (count (halves 1)) 1)
      (seq-merge (halves 0) (halves 1))
      (seq-merge (merge-sort (halves 0)) (merge-sort (halves 1))))))

(defn split-into-monotonics-helper [parts part a-seq]
  (let [inc-to-last (fn [elem coll] (reverse (cons elem (reverse coll))))]
    (cond
     (empty? a-seq) ;just include part to parts
       (inc-to-last part parts)
     (singleton? part) ;include the next elem to part automatically
       (split-into-monotonics-helper parts (inc-to-last (first a-seq) part)
                                     (rest a-seq))
     (< (first part) (last part)) ; are we going up or down?
       (if (> (last part) (first a-seq))
         (split-into-monotonics-helper (inc-to-last part parts)
                                       (cons (first a-seq) '()) (rest a-seq))
         (split-into-monotonics-helper parts (inc-to-last (first a-seq) part)
                                       (rest a-seq)))
     :else
       (if (< (last part) (first a-seq))
         (split-into-monotonics-helper (inc-to-last part parts)
                                       (cons (first a-seq) '()) (rest a-seq))
         (split-into-monotonics-helper parts (inc-to-last (first a-seq) part)
                                       (rest a-seq))))))

(defn split-into-monotonics [a-seq]
  ; call our helper function to do the work for us:
  ; parameters: parts (empty), part (inc. first elem), rest of the sequence
  (split-into-monotonics-helper '() (cons (first a-seq) '()) (rest a-seq)))

(defn num-of-perms [n]
  (if (> n 1) (* n (num-of-perms (dec n))) n))

(defn permutations-helper [result-seq n a-seq]
  (if (< 1 n)
    (let [tmp (a-seq (dec n))
          new-seq (assoc (assoc a-seq (dec n) (a-seq (- n 2))) (- n 2) tmp)]
      (permutations-helper (concat (rotations new-seq) result-seq) (dec n) a-seq))
    result-seq))

(defn permutations [a-set]
  (permutations-helper '() (count a-set) (into [] a-set)))

(defn powerset [a-set]
  [:-])

;O____o
