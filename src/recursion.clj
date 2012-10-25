(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (let [rest-of-coll (rest coll)]
  (if (empty? rest-of-coll)
    (first coll)
    (my-last rest-of-coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn rec-seq-max [seq-1 seq-2]
  (cond
   (singleton? seq-1) 2
   (singleton? seq-2) 1
   :else (rec-seq-max (rest seq-1)
                      (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (if (= (rec-seq-max seq-1 seq-2) 1)
    seq-1
    seq-2))

(defn rec-sequence [a-seq length b-seq]
  (let [first1 (first a-seq)
        count1 (count first1)
        rest-of (rest a-seq)]
    (if (empty? a-seq)
      b-seq
      (if (< count1 length)
        (rec-sequence rest-of length b-seq)
        (rec-sequence rest-of count1 first1)))))

(defn longest-sequence [a-seq]
  (rec-sequence a-seq -1 nil))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first1 (first a-seq)
          rest-of (rest a-seq)]
    (if (pred? first1)
      (cons first1
            (my-filter pred? rest-of))
      (my-filter pred? rest-of)))))

(defn sequence-contains? [elem a-seq]
  (let [first1 (first a-seq)]
  (cond
   (empty? a-seq) false
   (== first1 elem) true
   :else (sequence-contains? elem
                             (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [first1 (first a-seq)
        rest-of (rest a-seq)]
  (cond
   (empty? a-seq) a-seq
   (pred? first1) (cons first1
                      (my-take-while pred? rest-of))
   :else [])))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1)
            (empty? seq-2))
      []
     (cons (f
            (first seq-1)
            (first seq-2))
           (my-map f
                   (rest seq-1)
                   (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 2))
       (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons (do what-to-repeat)
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (let [decked (dec up-to)]
    (if (= up-to 0)
      []
      (cons decked (my-range decked)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (butlast a-seq)))))

(defn rotate [b-seq times]
  (if (zero? times)
    []
  (let [rotated (merge (vec (rest b-seq))
                      (first b-seq))]
    (cons rotated
          (rotate rotated
                  (dec times))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first1 (first a-seq)]
      (my-frequencies-helper
       (assoc freqs first1 (inc (get freqs first1 0)))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies [a-map]
  (let [first1 (first a-map)]
  (if (empty? a-map)
    []
    (into (repeat (val first1)
                  (key first1)) 
          (un-frequencies (rest a-map))))))

(defn rec-my-face [n coll]
  (if (< n 1)
    coll
    (rec-my-face (dec n) (drop-last coll))))

(defn my-take [n coll]
  (rec-my-face (- (count coll) n) coll))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (vec (rest coll)))))

(defn rec-rec-halve [a-seq]
  (if (empty? a-seq)
    []
    (cons (first a-seq)
          (rec-rec-halve (rest a-seq)))))

(defn rec-halve [a-seq n]
  (if (< n 1)
    (vector (rec-rec-halve a-seq))
    (cons (first a-seq)
          (rec-halve (rest a-seq)
                     (dec n)))))

(defn halve [a-seq]
  (rec-halve a-seq
             (int (/ (count a-seq) 2))))

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])