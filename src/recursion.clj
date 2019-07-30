(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll)
       (empty? (rest coll))))

(defn my-last [coll]
  (cond 
    (empty? coll)
      nil 
   (singleton? coll)
     (first coll)
   :else
     (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond 
    (empty? a-seq) 
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
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
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
       a-seq))      

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (count a-seq) (count b-seq))
      (if (== (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false)
    :else
      false))
         

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      ()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n)
          0
        (= n 1)
          1
        :else 
          (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      ()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat ))))

(defn my-range [up-to]
  (if (zero? up-to)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
        (cons () ()) 
        (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
        (cons () ()) 
        (cons a-seq (inits (take (dec (count a-seq)) a-seq)))))

 ; (reverse (tails (reverse a-seq))))


(defn rotations [a-seq]
  (let [rotate (fn [a-seq] (concat (rest a-seq) (list (first a-seq))))
        repeat-rotate (fn repeat-rotate [how-many-times rot-seq]
         (if (zero? how-many-times)
           ()
           (cons rot-seq (repeat-rotate (dec how-many-times) (rotate rot-seq)))))
        ]
  (if (empty? a-seq)
    (cons () ())
    (repeat-rotate (count a-seq) a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
     freqs
     (if (contains? freqs (first a-seq))
       (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq) ))) (rest a-seq))
       (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper (hash-map) a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) 
    []
    (let [[map-key map-value] (first a-map)]
      (concat (repeat map-value map-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
  (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
      coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split (int (/ (count a-seq) 2))]
    (vector (my-take split a-seq) (my-drop split a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond 
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    :else
       (if (< (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [halved (halve a-seq)]
  (if (empty? (rest a-seq))
    a-seq
    (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))


(defn split-into-monotonics [a-seq]
  (let [monotonic (fn [x] (if (empty? x)
                             true
                             (or (apply > x)
                                 (apply < x))))
        m-seq (last (take-while monotonic (reverse (inits a-seq))))]
    (if (empty? a-seq)
      a-seq
     (cons m-seq (split-into-monotonics (drop (count m-seq) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    (cons () ())
    (mapcat (fn [x] (map (fn [y] (cons (first x) y)) (permutations (rest x)))) (rotations a-set))))

(defn powerset [a-set]
  (if (empty? a-set)
      (set (cons (set ()) ()))
      (set (cons (set a-set) (mapcat (fn [x] (powerset (set (rest x)))) (rotations a-set))))))

