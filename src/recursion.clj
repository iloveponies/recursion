(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [a (first a-seq)
          b (max-element (rest a-seq))]
      (max a (if (= b nil) a b)))))

(defn seq-max [seq-1 seq-2]
  (let [a (count seq-1)
        b (count seq-2)]
    (if (> a b) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) 
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))
    

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (== (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
(cond
  (empty? a-seq) a-seq
  (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  :else (empty a-seq)))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    (empty a-seq)
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (and (not (nil? (first a-seq))) (not (nil? (first b-seq))))
      (and (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (empty seq-1)
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ();; Return an empty sequence
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    ();; return an empty sequence
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons (empty a-seq) ())
    (cons (my-take-while (complement nil?) a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [swap-elem (fn [a-seq] (reverse a-seq))]
    (map swap-elem (reverse (tails (reverse a-seq))))))

(defn rotate [a-seq, n]
  (if (== n 1)
    (empty a-seq)
    (let [new-seq (concat (rest a-seq) [(first a-seq)])]
      (cons new-seq (rotate new-seq (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (seq [()])
    (cons (seq a-seq) (rotate a-seq (count a-seq)))))


(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (get freqs k)
          increment-value (fn [v] (if (nil? v)
                                    1
                                    (inc v)))]
    (my-frequencies-helper (assoc freqs k (increment-value v)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (empty seq)
    (let [k (first (first a-map))
          v (second (first a-map))]
      (concat (my-repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== n 0) (== (count coll) 0))
    (empty coll)
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== n 0) (== (count coll) 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-index (int (/ (count a-seq) 2))
        first-half (my-take half-index a-seq)
        second-half (my-drop half-index a-seq)]
    (cons first-half [second-half])))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (and (nil? a) (nil? b)) nil
      (nil? a) b-seq
      (nil? b) a-seq
      :else (if (> a b)
              (cons b (seq-merge a-seq (rest b-seq)))
              (cons a (seq-merge (rest a-seq) b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (== (count a-seq) 1) a-seq
    :else (let [[t-seq1, t-seq2] (halve a-seq)]
            (seq-merge (merge-sort t-seq1) (merge-sort t-seq2)))))



(defn get-greater [a-seq]
  (let [a (first a-seq)
        b (second a-seq)]
    (if (nil? b)
      [a]
      (if (>= b a)
        (cons a (get-greater (rest a-seq)))
        [a]))))

(defn get-lesser [a-seq]
  (let [a (first a-seq)
        b (second a-seq)]
    (if (nil? b)
      [a]
      (if (<= b a)
        (cons a (get-lesser (rest a-seq)))
        [a]))))

(defn rising? [a-seq]
  (if (empty? a-seq)
    false
    (let [a (first a-seq)
          b (second a-seq)]
      (if (nil? b)
        false
        (>= b a)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (if (rising? a-seq)
      (let [l (get-greater a-seq)]
        (cons l (split-into-monotonics (my-drop (count l) a-seq))))
      (let [l (get-lesser a-seq)]
        (cons l (split-into-monotonics (my-drop (count l) a-seq)))))))



;;------------------------------------------------------------------
;; 1. Partition the input into a first element and take the rest as is
;; 2. Combine the first element with the permutations of the rest elements.
;;------------------------------------------------------------------
(defn permutations [a-set]
  (let [permutations-helper (fn [[fst-value & rest-values]]
        (map (fn [a-seq] (cons fst-value a-seq)) (permutations rest-values)))]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (list a-set)
    :else (apply concat (map permutations-helper (rotations a-set))))))


;;------------------------------------------------------------------
;; 1. map all the 'tails' and 'inits' as sets, to remove doubles...
;; 2. merge the two sets
;; 3. return a set of sets
;;------------------------------------------------------------------
(defn power-helper [a-set]
  (let [init (inits a-set)
        tail (tails a-set)]
  (set (concat (map set tail) (map set init)))))

;;------------------------------------------------------------------
;; 1. map all the permutations of the set with the power-helper function
;; 2. concatenate all the results into one sequence
;; 3. return a set instead of the sequence
;;------------------------------------------------------------------
(defn powerset [a-set]
  (set (apply concat (map power-helper (permutations a-set)))))



