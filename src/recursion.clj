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
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    (singleton? (rest a-seq)) (max (first a-seq) (second a-seq))
    :else (max-element (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    (singleton? (rest a-seq)) (seq-max (first a-seq) (second a-seq))
    :else (longest-sequence (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
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
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))) 

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
  (if ((complement pos?) how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
    (if ((complement pos?) up-to)
      ()
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (seq (set (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head       (first a-seq)
          head-value (get freqs head)
          new-value  (if (nil? head-value)
                       1
                       (inc head-value))
          new-freqs  (assoc freqs head new-value)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [head     (first a-map)
          head-key (first head)
          head-val (second head)]
      (concat (repeat head-val head-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if
    (or (empty? coll) ((complement pos?) n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if
    (or (empty? coll) ((complement pos?) n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size  (int (/ (count a-seq) 2))]
    [(my-take size a-seq) (my-drop size a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-head (first a-seq)
        b-head (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) ()
      (nil? a-head)     (cons b-head (seq-merge a-seq (rest b-seq)))
      (nil? b-head)     (cons a-head (seq-merge (rest a-seq) b-seq))
      (< a-head b-head) (cons a-head (seq-merge (rest a-seq) b-seq))
      :else             (cons b-head (seq-merge a-seq (rest b-seq))))))                  

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq 
    (let [half (halve a-seq)]
      (seq-merge (merge-sort (first half)) (merge-sort (second half))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

