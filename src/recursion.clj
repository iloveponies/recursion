(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (== 1 (count coll))
    true
    false))

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
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
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
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (= (count a-seq) (count b-seq)) ;pituus
      (if (and (singleton? a-seq) (singleton? b-seq)) ; jos vikat
        (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq)))
      false)))

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
   (< n 1) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   (= 1 how-many-times) (list what-to-repeat)
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= up-to 0) ()
   (= up-to 1) (list 0)
   :else  (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (seq a-seq) (inits (drop-last a-seq)))))


(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (partition (count a-seq) 1 (concat a-seq (drop-last a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (cond
     (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
     :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ""
    (let [amount (first (vals a-map))
          value (first (keys a-map))]

       (concat (repeat amount value) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (= n 0) (list)
   (< (count coll) n) (seq coll)
   :else (cons (first coll ) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (= n 0) coll
   (or (= (count coll) 0) (< (count coll) n)) (list)
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (< (count a-seq) 2)
    [() a-seq]
    (let [size (int (/ (count a-seq) 2))]
      (vector (my-take size a-seq) (my-drop size a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? b-seq) a-seq
   (empty? a-seq) b-seq
   :else (if (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) (seq b-seq)))
      (cons (first b-seq) (seq-merge (seq a-seq) (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge
     (merge-sort (get (halve a-seq) 0))
     (merge-sort (get (halve a-seq) 1)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

