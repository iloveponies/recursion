(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (apply max a-seq))

(defn seq-max [seq-1 seq-2]
  (if (< (count seq-1) (count seq-2))
    seq-2
    seq-1
  ))

(defn longest-sequence [a-seq]
  (apply seq-max a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq))
    )))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while (rest a-seq)))
      '()
    )))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (== (count a-seq) (count b-seq))
    false
    (if (== (first a-seq)(first b-seq))
      (seq= (rest a-seq)(rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons 
      (f (first seq-1) (first seq-2)) 
      (f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib ((dec (dec n)))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (cons (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty a-seq)
    '()
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (tails (reverse a-seq)))

(defn rotations [a-seq]
)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? (set (keys freqs)) (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
    )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [key] (my-repeat (key a-map) key)) (keys a-map))))

(defn my-take [n coll]
  (if (= (str n) (str (count coll)))
    coll
    (my-take n (butlast coll))))

(defn my-drop [n coll]
  (if (= (str n) (str 0))
    coll	
    (my-drop ((dec n) (rest coll)))))

(defn halve [a-seq]
  (if (< (count a-seq) 2)
    ['(), a-seq]
    (vector 
      (my-take (int (/ (count a-seq) 2)) a-seq)
      (my-drop (int (/ (count a-seq) 2)) a-seq)
    )))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))  '()
    (empty? a-seq) (cons (
        first b-seq) (seq-merge '() (rest b-seq))) 
    (empty? b-seq) (cons (
        first a-seq) (seq-merge '() (rest a-seq)))
    :else (if (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
    )))

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq)))
    )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

