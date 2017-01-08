(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

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
      (max (first a-seq)
        (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
        (longest-sequence (rest a-seq))))))

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
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
     (cond
       (empty? a-seq)
        a-seq 
       (pred? (first a-seq))
         (cons (first a-seq) (my-take-while pred? (rest a-seq)))
       :else
         []))

(defn my-drop-while [pred? a-seq]
 (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1)(empty? seq-2))
      []
    :else
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (zero? n)
      0
    (== 1 n)
      1
    :else
      (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if ((complement pos?) how-many-times)
     []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if ((complement pos?) up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [len items]
  (if (or (zero? len)(empty? items))
    nil
    (let [new-items (concat (rest items) [(first items)])]
      (concat 
        [new-items]
        (rotations-helper (dec len) new-items)))))

(defn rotations [a-seq]
    (if (empty? a-seq)
      '(())
      (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [cur (first a-seq)
         newval (if (contains? freqs cur)
                   (inc (get freqs cur))
                   1)]
   (cond 
    (empty? a-seq)
      freqs
    :else
      (my-frequencies-helper 
        (assoc freqs cur newval) 
        (rest a-seq)))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (if (empty? a-map)
   nil ;(list a-map)
   (let [a-key (first (first a-map))
         times (second (first a-map))]
     (concat (repeat times a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-size (count a-seq)
        first-half-size (int (/ seq-size 2))]
    (concat (list (my-take first-half-size a-seq)) (list (my-drop first-half-size a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq))
      '()
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    :else
      (let [nextval (min (first a-seq) (first b-seq))]
        (if (= nextval (first a-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond 
    (or (empty? a-seq) (= 1 (count a-seq)))
      a-seq
    :else
      (let [halfs (halve a-seq)]
      (seq-merge (merge-sort (first halfs)) (merge-sort (second halfs))))))

(defn split-into-monotonics [a-seq]
 [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

