(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))(empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
   (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
          (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
  (if (pred? (first a-seq))
    (cons (first a-seq)
      (my-take-while pred? (rest a-seq)))
    '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    a-seq)))

(defn seq= [seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) true
    (not (= (first seq-1) (first seq-2))) false
    :else (seq= (rest seq-1) (rest seq-2))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
   (cons (f (first seq-1) (first seq-2))(my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

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
    (list a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (for [n (range (inc (count a-seq)))]
    (take n a-seq)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(rotations [])

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
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
   (if (empty? a-map)
     ()
     (concat (my-repeat (first (rest (first a-map))) (first (first a-map)))(un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (and (not (empty? coll))(> n 0))
    (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
    (if (not (empty? coll))
      (if (> n 0)
        (my-drop (dec n) (rest coll))
        coll)
      ()))

(defn halve [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (vector '() a-seq )
    (let [split-point (int (/ (count a-seq) 2))]
      (vector (my-take split-point a-seq) (my-drop split-point a-seq)))))

(defn seq-merge [a-seq b-seq]
  (if-not (and (empty? a-seq) (empty? b-seq))
    (if (empty? a-seq)
      b-seq
      (if (empty? b-seq)
        a-seq
        (if (< (first a-seq) (first b-seq))
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
          (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))))

(defn merge-sort [a-seq]
  (let [halve-seq (halve a-seq)]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort (first halve-seq))(merge-sort (first (rest halve-seq)))))))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

