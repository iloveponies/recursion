(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (and (nil? (second coll))
       (not (empty? coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))
    ; Vähän kikkaileva toteutus

(defn rotations-helper [n a-seq]
  (if (= n 0)
    '()
    (cons a-seq 
          (rotations-helper (dec n)  (concat (rest a-seq) 
                                            [(first a-seq)])))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    ['()]
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs
                                  (first a-seq)
                                  (inc (get freqs (first a-seq) 0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    {}
    (concat (repeat (val (first a-map)) (key (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    '()
    (cons (first coll) 
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    (cons (my-take midpoint a-seq) [(my-drop midpoint a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[half1 half2] (halve a-seq)]
      (seq-merge (merge-sort half1) (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations-helper [pre-set a-set]
  (if (= (count a-set) 1)
    (conj pre-set (first a-set))
    (let [do-stuff (fn [x] (permutations-helper (conj pre-set (first x)) (rest x)))]
      (map do-stuff (rotations a-set)))))

(defn permutations [a-set]
  (permutations-helper [] a-set))

(defn powerset [a-set]
  [:-])

