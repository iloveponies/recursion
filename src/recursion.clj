(ns recursion)

(defn product [coll]
  (if(empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
      false
      (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (not (empty? a-seq))
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
    (if (empty? a-seq)
      a-seq
      (let [fst (first a-seq)
            rst (my-filter pred? (rest a-seq))]
          (if (pred? fst)
              (cons fst rst)
                rst))))

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq)
         false
     (== elem (first a-seq))
         true
     :else
         (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [fst (first a-seq)]
      (if (pred? fst)
        (cons fst (my-take-while pred? (rest a-seq)))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq))
           true
     (or (empty? a-seq) (empty? b-seq))
           false
     (== (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq))
     :else
           false
   ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
         (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== n 0) 0
   (== k 0) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (< n 2) n
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
      '()))

(defn my-range [up-to]
  (if (> up-to 0)
      (cons (dec up-to) (my-range (dec up-to)))
      '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          found (find freqs item)
          old_value (second found)
          helper (fn [value] (my-frequencies-helper (assoc freqs item value) (rest a-seq)))]
       (if found
          (helper (inc old_value))
          (helper 1)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[key cnt] (first a-map)]
      (concat
           (repeat cnt key)
           (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
      '()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (> n 0)
      (my-drop (dec n) (rest coll))
      coll))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
      [(my-take x a-seq) (my-drop x a-seq)]))


(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if(< (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (let [sq1 (first (halve a-seq))
          sq2 (second (halve a-seq))]
       (seq-merge (merge-sort sq1) (merge-sort sq2)))
    a-seq))

(defn monotonic [n a-seq]
   (if (= n (count a-seq))
     a-seq
     (let [test-seq (my-take (inc n) a-seq)
           is_inc? (apply <= test-seq)
           is_dec? (apply >= test-seq)]
     (if (or is_inc? is_dec?)
            (monotonic (inc n) a-seq)
            (my-take n a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [sq (monotonic 0 a-seq)
          left (my-drop (count sq) a-seq)]
          (cons sq (split-into-monotonics left)))))


(defn permutations [a-set]
  (if(empty? a-set)
    '(())
    (if (= (count a-set) 2)
        (seq [(first a-set ) (second a-set)])
        (loop [i (count a-set) res '()]
          ()))))



(defn powerset [a-set]
  [:-])


























