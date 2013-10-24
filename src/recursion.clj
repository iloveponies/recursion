(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

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
    (if (not (= nil seq-2))
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
     (first a-seq)
     (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
    false
   (= (first a-seq) elem)
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
     '()))

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
   (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
   :else
    false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0)
    0
    (<= n 2)
    1
    :else
     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn all-but-last [a-seq]
  (if (empty? (rest a-seq))
    '()
    (cons (first a-seq) (all-but-last (rest a-seq)))))

(defn next-rotation [a-seq]
  (let [last-e (my-last a-seq)
        but-last (all-but-last a-seq)]
    (cons last-e but-last)))

(defn rotation-helper  [n a-seq]
  (if (= (count a-seq) n)
    '()
    (let [new-n (+ n 1)]
      (cons a-seq (rotation-helper new-n (next-rotation a-seq))))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq)
    '()
    (singleton? a-seq)
    (seq a-seq)
    :else
    (rotation-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (get freqs (first a-seq))]
      (if (nil? f)
       (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
       (my-frequencies-helper (assoc freqs (first a-seq) (+ f 1)) (rest a-seq))))))

(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map]
  (if (empty? a-map)
    '()
    (cons (repeat (first (vals a-map)) (first (keys a-map))) (un-frequencies-helper (rest a-map)))))

(defn un-frequencies [a-map]
  (apply concat (un-frequencies-helper a-map)))

(defn my-take [n coll]
  (cond
   (empty? coll)
    '()
   (= n 0)
    '()
   :else
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
   (vector (my-take x a-seq) (my-drop x a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
    '()
   (empty? a-seq)
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq)
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
    (if (> (first a-seq) (first b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (seq-merge (get (halve a-seq) 1) (get (halve a-seq) 0))
    (seq-merge (merge-sort (get (halve a-seq) 1)) (merge-sort (get (halve a-seq) 0)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


; ^_____^



