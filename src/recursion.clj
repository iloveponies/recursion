(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not(empty? coll))))

(defn my-last [coll]
  (if (empty? coll)
    (first coll)
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) []
    :else (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (not (pred? (first a-seq))) []
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and(empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    :else (and
      (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2] ;; Isn't this basically zipWith?
  (cond
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    (= k 1) n
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    (= n 2) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) []
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) ()
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) [[]]
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn init [a-seq]
  (take (- (count a-seq) 1) a-seq))

(defn rotaten [n a-seq]
  (cond
    (= n 0) (cons (last a-seq) (init a-seq))
    :else
      (rotaten (- n 1) (cons (last a-seq) (init a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (for [x (range (count a-seq))]
      (rotaten x a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (cond
      (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1)) (rest a-seq))
      :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (flatten (map (fn [[x y]] (repeat y x)) a-map)))

(defn mthelp[ret n coll]
  (cond
    (= n 0) ret
    :else (mthelp
      (conj ret (first coll))
      (- n 1)
      (rest coll))))

(defn my-take [n coll]
  (mthelp [] (min n (count coll)) coll))

(defn my-drop [n coll]
  (cond
    (= n 0) coll
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (cons
    (take (int (/ (count a-seq) 2)) a-seq )
    [(drop (int (/ (count a-seq) 2)) a-seq)]))

(defn smhelp [ret a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ret
    (empty? b-seq) (concat ret a-seq)
    (empty? a-seq) (concat ret b-seq)
    (<= (first a-seq) (first b-seq))
      (smhelp
        (concat ret [(first a-seq)])
        (rest a-seq)
        b-seq)
    :else (smhelp
      (concat ret [(first b-seq)])
      a-seq
      (rest b-seq))
    ))

(defn seq-merge [a-seq b-seq]
  (smhelp [] a-seq b-seq))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1) a-seq
    :else (seq-merge
      (merge-sort
        (first (halve a-seq)))
      (merge-sort
        (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

