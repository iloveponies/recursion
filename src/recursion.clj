(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (rest a-seq)] 
      (if (pred? fst)
        (cons fst (my-filter pred? rst))
        (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (< up-to 1) '()
    :else (let [n (dec up-to)]
            (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotate-times [[n xs]]
  (let [rotate (fn [xs] (concat (rest xs) [(first xs)]))]
    (if (zero? n)
      xs
      (rotate-times [(dec n) (rotate xs)]))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [len (count a-seq)
          pairs (map vector (range 0 len) (repeat len a-seq))]
      (map rotate-times pairs))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          rst (rest a-seq)
          newfreqs (assoc freqs
                          fst (inc (get freqs fst 0)))]
      (my-frequencies-helper newfreqs rst))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat 
    (map (fn [[val n]] 
           (repeat n val))
      a-map)))
                       

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split (int (/ (count a-seq) 2))]
    [(my-take split a-seq) (my-drop split a-seq)]))

(defn seq-merge-helper [out xs ys]
  (cond
    (empty? xs) (concat out ys)
    (empty? ys) (concat out xs)
    :else (let [x (first xs)
                y (first ys)]
            (if (< x y)
              (seq-merge-helper
                (concat out [x])
                (rest xs)
                ys)
              (seq-merge-helper
                (concat out [y])
                xs
                (rest ys))))))
                                  
            
(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq))) ; 0 or 1 length: sorted
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic? [a-seq]
  (or (empty? a-seq) (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic-inits (filter monotonic? (inits a-seq))
          longest (last monotonic-inits)
          n (count longest)
          tail (drop n a-seq)]
      (cons longest (split-into-monotonics tail)))))

(defn permutations [a-set]
  (if (zero? (count a-set))
    '(())
    (let [n (count a-set)
          ns (range 0 n)
          pick (fn [i] (first (drop i a-set)))
          others (fn [i] (concat (take i a-set) (drop (inc i) a-set)))
          pair (fn [i] [(pick i) (others i)])
          pairs (map pair ns)
          prepend (fn [num] (fn [xs] (cons num xs)))
          pair->perms (fn [[num rst]] (map (prepend num) (permutations rst)))]
      (apply concat (map pair->perms pairs)))))

(defn powerset [a-set]
  [:-])

