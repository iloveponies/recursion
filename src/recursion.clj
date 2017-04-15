(ns recursion)

(defn product [coll]
  (if
    (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if
    (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if
    (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if
    (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if
    (empty? a-seq) a-seq
    (if
      (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (or (empty? a-seq) (empty? b-seq)) (or (not (empty? a-seq)) (not (empty? b-seq)))) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2))
    ()
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to) ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if
    (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let
    [b-seq (reverse a-seq)]
    (if
      (empty? b-seq) '(())
    (cons (reverse (seq b-seq)) (inits (reverse (rest b-seq)))))))

(defn rot2 [n a-seq]
  (if
    (zero? n)
    ()
    (cons (concat a-seq)
          (rot2
           (dec n)
           (reverse
            (conj
             (reverse (rest a-seq))
             (first a-seq)))))))

(defn rotations [a-seq]
  (if
    (empty? a-seq)
    '(())
    (rot2 (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper
                                    (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                                    (rest a-seq))
   :else (my-frequencies-helper
          (assoc freqs (first a-seq) 1)
          (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (cond
   (empty? a-map) a-seq
   (== 1 (last (first a-map)))(un-frequencies-helper
                                 (reverse (conj (reverse a-seq) (first (first a-map))))
                                 (dissoc a-map (first (first a-map))))
   :else (un-frequencies-helper
         (reverse (conj (reverse a-seq) (first (first a-map))))
          (assoc a-map (first (first a-map)) (dec (last (first a-map)))))))


(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (let
    [x (apply vector (reverse (inits coll)))]
    (if
     (>= n (count coll)) coll
     (get x n))))

(defn my-drop [n coll]
  (let
    [x (apply vector (tails coll))]
    (if
      (>= n (count coll)) ()
      (get x n))))

(defn halve [a-seq]
  (let
    [x (int (/ (count a-seq) 2))]
    [(my-take x a-seq) (my-drop x a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? a-seq) ()
    (let
      [[x y] (halve a-seq)]
      (if
        (== 1 (count a-seq)) (seq-merge y ())
        (seq-merge (merge-sort x) (merge-sort y))))))

(defn split-into-monotonics [a-seq]
  ())

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

