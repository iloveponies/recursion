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
  (if (< (count seq-2)
         (count seq-1))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
   nil
   (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)
        (empty? b-seq)) true
   (or (empty? a-seq)
       (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1)
        (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

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
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [rotation (fn rotation [i a-seq]
                   (if (< i (count a-seq))
                    (cons (seq a-seq)
                          (rotation (inc i)
                                    (concat (rest a-seq)
                                            (vector (first a-seq)))))))]
    (if (empty? a-seq)
      '(())
      (rotation 0 a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [i (if (contains? freqs (first a-seq))
              (inc (get freqs (first a-seq)))
              1)
          new-freqs (assoc freqs (first a-seq) i)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [fst (first a-map)
          a-key (first fst)
          times (second fst)
          a-seq (repeat times a-key)]
      (concat a-seq (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1)
           (empty? coll))
     '()
     (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [i (int (/ (count a-seq) 2))]
    [(my-take i a-seq) (my-drop i a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (or (empty? a-seq)
       (empty? b-seq)) (concat a-seq b-seq)
   (< (first a-seq)
      (first b-seq)) (cons
                      (first a-seq)
                      (seq-merge (rest a-seq) b-seq))
   :else (cons
          (first b-seq)
          (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq) '()
   (or (apply <= a-seq)
       (apply >= a-seq)) (cons a-seq '())
   :else
   (let [non-empty-inits (rest (inits a-seq))
         incr-seq? (fn [b-seq] (apply <= b-seq))
         decr-seq? (fn [b-seq] (apply >= b-seq))
         incr (take-while incr-seq? non-empty-inits)
         decr (take-while decr-seq? non-empty-inits)
         monot (first (reverse (seq-max incr decr)))
         tail (drop (count monot) a-seq)]
     (cons monot (split-into-monotonics tail)))))

(defn permutations [a-set]
  (let [many-cons (fn [an-element seq-seq]
                    (let [cons-seq-with-element (fn [a-seq]
                                                  (cons an-element a-seq))]
                      (map cons-seq-with-element seq-seq)))
        rec (fn [b-seq]
             (many-cons (first b-seq) (permutations (rest b-seq))))]
    (if (empty? a-set)
      '(())
      (apply concat (map rec (rotations a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (cons a-set #{})
    (let [subsets-minus-one-element (map set (map rest (rotations a-set)))]
      (set (cons (set a-set) (apply concat(map powerset subsets-minus-one-element)))))))








