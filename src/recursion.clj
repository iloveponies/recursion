(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
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
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

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
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (concat (tails (rest a-seq)) (list a-seq))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (concat (inits (reverse (rest (reverse a-seq)))) (list a-seq))))

(defn rotations [a-seq]
  (map (fn [n]
         (concat (drop n a-seq) (take n a-seq)))
       (range (max 1 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs
                             (first a-seq)
                             (+ 1 (if (get freqs (first a-seq))
                                    (get freqs (first a-seq))
                                    0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (repeat (get (first a-map) 1)
                    (get (first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (zero? n)
    '()
    (if (empty? coll)
      coll
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
    (vector (my-take x a-seq) (my-drop x a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (and (first a-seq) (first b-seq))
    (if (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))
    (if (first a-seq)
      (cons (first a-seq) (rest a-seq))
      (if (first b-seq)
        (cons (first b-seq) (rest b-seq))
        b-seq))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (get halves 0))
                 (merge-sort (get halves 1))))))

(defn split-into-monotonics-helper [result current a-seq]
  (if (empty? a-seq)
    (if (empty? current)
      result
      (conj result current))
    (if (< (count current) 2)
      (split-into-monotonics-helper
        result
        (conj current (first a-seq))
        (rest a-seq))
      (if (or (apply <= (conj current (first a-seq)))
              (apply >= (conj current (first a-seq))))
        (split-into-monotonics-helper
          result
          (conj current (first a-seq))
          (rest a-seq))
        (split-into-monotonics-helper
          (conj result current)
          (conj [] (first a-seq))
          (rest a-seq))))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper [] [] a-seq))

(defn permutations-helper [current remaining]
  (if (empty? remaining)
    (list current)
    (apply concat
           (map (fn [x]
                  (permutations-helper
                    (conj current x)
                    (disj remaining x)))
                remaining))))

(defn permutations [a-set]
  (permutations-helper '() (set a-set)))

(defn powerset-helper [result remaining]
  (if (empty? remaining)
    (conj result remaining)
    (apply clojure.set/union
           (conj result remaining)
           (map (fn [x]
                  (powerset-helper
                    result
                    (disj remaining x)))
                remaining))))

(defn powerset [a-set]
  (powerset-helper #{} (set a-set)))
