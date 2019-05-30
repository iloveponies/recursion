(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1 
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))(empty? (rest coll))))

(defn my-last [coll]
  (if (nil? (fnext coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [[head & tail] a-seq]
    (if (empty? tail)
      head
      (recur (cons (max head (first tail)) (rest tail))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [[head & tail] a-seq]
    (if (empty? tail)
      head
      (recur (cons (seq-max head (first tail)) (rest tail))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
      (if (pred? head) 
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '() ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false 
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond 
    (zero? n) 0
    (zero? k) 1
    (= k 1) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    (> n 1) (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    [[]]
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons (sequence a-seq) (inits (pop a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (map 
     (fn [n _]
       (concat (drop n a-seq) (take n a-seq)))
     (range (count a-seq)) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [a-key (first (first a-map))
          a-val (second (first a-map))]
      (concat (repeat a-val a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond 
    (empty? coll) '()
    (zero? n) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond 
    (empty? coll) '()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (take half a-seq) (drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) [[]]
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
