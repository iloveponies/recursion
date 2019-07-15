(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
     :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
     :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
     (empty? a-seq) a-seq
     (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq) false
     (== elem (first a-seq)) true
     :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq)  a-seq
     (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)  a-seq
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
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
     (zero? k) 1
     (= k 1) n
     :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
     (zero? n) 0
     (= 1 n)   1
     :else (+ (fib (dec n))
              (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
      []
      (cons what-to-repeat
            (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
     []
     (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      (cons a-seq [])
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq n]
    (if (zero? n)
        ()
        (let [new (concat (rest a-seq) [(first a-seq)])]
          (cons new (rotations-helper new (dec n))))))

(defn rotations [a-seq]
    (if (empty? a-seq)
        '(())
         (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
     (empty? a-seq) freqs
     (contains?  freqs  (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
     :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [cha n]
  (if (zero? n)
      ()
      (cons cha (un-frequencies-helper cha (dec n)))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
      ()
      (let [fir  (first a-map)
            res  (rest a-map)]
        (concat (repeat (get fir 1) (get fir 0)) (un-frequencies res)))))


(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
      ()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
      coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half  (int (/ (count a-seq) 2))]
       [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq))  ()
     (empty? a-seq)   (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
     (empty? b-seq)   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
      a-seq
      (let [halves (halve a-seq)]
           (seq-merge (merge-sort (first halves))
                      (merge-sort(second halves))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
      ()
      (let [m (first (filter monotonic? (inits a-seq)))]
        (cons m (split-into-monotonics (drop (count m) a-seq))))))



(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

