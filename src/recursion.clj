(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) 
         (product (rest coll)))))

(defn singleton? [coll]
  (if (and
       (not (empty? coll))
       (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (cond 
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

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
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
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
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond 
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0) 
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails-helper [a-seq]
  (if (empty? a-seq)
    '()
    (cons a-seq (tails-helper (rest a-seq)))))

(defn tails [a-seq]
  (map rest (tails-helper (cons '() a-seq))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
      (reverse (rest (map concat (tails a-seq) (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper 
     (assoc freqs (first a-seq) (inc (freqs (first a-seq) 0))) 
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [item (first (first a-map))
          count (first (rest (first  a-map)))]
      (concat 
       (repeat count item) 
       (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [boundary (int (/ (count a-seq) 2))]
    (cons (my-take boundary a-seq) (list (my-drop boundary a-seq))))) 

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= a b) 
       (cons a 
             (seq-merge (rest a-seq) b-seq))
     (> a b) 
       (cons b
             (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) 
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (or 
   (apply <= a-seq)
   (apply >= a-seq)))

(defn first-monotonic-seq [a-seq]
  (first 
   (reverse
    (take-while monotonic? 
                (drop 1 
                      (inits a-seq))))))

(defn split-into-monotonics [a-seq]
  (let [first-seq (first-monotonic-seq a-seq)]
    (if (empty? a-seq)
      '()
      (cons 
       first-seq
       (split-into-monotonics (drop (count first-seq) a-seq))))))

(defn permutations [a-set]
  (if (or 
       (empty? a-set)
       (singleton? a-set))
    (list a-set) 
    (mapcat 
     (fn [x] 
       (map (fn [y] (cons (first x) y))
            (permutations (rest x))))
     (rotations a-set))))

(defn powerset [a-set]
  (if (empty? a-set)
    (list '())
    (let [ps (powerset (rest a-set))]
      (clojure.set/union 
       ps
       (map (fn [x] (cons (first a-set) x)) ps)))))

