(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll) (empty? (rest coll))))

(defn my-last [coll]
 (if (empty? (rest coll))
   (first coll)
   (my-last(rest coll))))

(defn max-element [a-seq]
  (if(empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (max-element (rest a-seq)) (first a-seq)))))

(defn seq-help [seq-1 seq-2]
    (if (and ((complement empty?) seq-1) ((complement empty?) seq-2))
        (seq-help (rest seq-1) (rest seq-2))
        (empty? seq-1)))

(defn seq-max [seq-1 seq-2]
  (if (seq-help seq-1 seq-2)
    seq-2
    seq-1))

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
      (if (= (first a-seq) elem)
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
 (if (and (empty? seq-1) (empty? seq-2))
   true
   (if (= (first a-seq) (b-seq))
     (seq= (rest a-seq) (rest b-seq))
     false)))

(defn my-map [f seq-1 seq-2]
  (let [x nil]
    (curser f seq-1 seq-2 x)))

(defn curser [f seq-1 seq-2 seq-0]
  (if (or (empty? seq-1) (empty? seq-2))
    (reverse seq-0)
    (curser f (rest seq-1) (rest seq-2) (conj seq-0 (f (first seq-1) (first seq-2))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (== n 0)
    0
    (if (<= n 2)
     1
     (+ (fib (dec (dec n))) (fib (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [x how-many-times y what-to-repeat]
    (if (<= x 0)
      nil
      (cons y (my-repeat (dec x) y)))))

(defn my-range [up-to]
  (if (<= up-to 0)
    nil
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  nil)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc-in freqs [(first a-seq)] (inc (get-in freqs [(first a-seq)]))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (let [x (nil)]   
  (my-frequencies-helper x a-seq)))

(defn un-frequencies [a-map]
   (let [elem (first a-map)
        first-key (first elem)
        first-val (second elem)]
    (if(empty? a-map)
      []
      (concat (repeat first-val first-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    nil
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [l (int (/ (count a-seq) 2))]
    [(my-take l a-seq) (my-drop l a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? b-seq) a-seq
    (empty? a-seq) b-seq
    (<= (first a-seq) (first b-seq))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
  :else
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
   (cond
     (> 2 (count a-seq))
     a-seq
   :else
      (let[[x y](halve a-seq)]
        (seq-merge (merge-sort x)(merge-sort y)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])