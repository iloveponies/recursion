(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
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
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (reduce seq-max a-seq))) ; Is this cheating?

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (empty? b-seq) (not (empty? a-seq))) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

; Easier but cheating.. (apply * (repeat n k))
(defn power [n k]
  (if (> k 0)
    (* n (power n (dec k)))
    1))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [rot (fn [a-seq n]
                (concat (nthrest a-seq n) (take n a-seq)))]
      (map-indexed #(rot %2 %1) (repeat (count a-seq) a-seq)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (get freqs k 0)]
      (my-frequencies-helper (assoc freqs k (inc v)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce (fn [a [b c]] (concat a (repeat c b))) '() a-map))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    [(my-take div a-seq) (my-drop div a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a (first a-seq)
               b (first b-seq)]
           (if (< a b)
             (cons a (seq-merge (rest a-seq) b-seq))
             (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2) a-seq ; Already sorted
   :else  (let [[a b] (halve a-seq)]
            (seq-merge (merge-sort a) (merge-sort b)))))

(defn take-adj-while [pred? a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [a (first a-seq)
          b (second a-seq)]
      (if (pred? a b)
        (cons a (take-adj-while pred? (rest a-seq)))
        (list a)))))

(defn drop-adj-while [pred? a-seq]
  (if (< (count a-seq) 2)
    '()
    (let [a (first a-seq)
          b (second a-seq)]
      (if (pred? a b)
        (drop-adj-while pred? (rest a-seq))
        (rest a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a (take-adj-while <= a-seq)
          b (take-adj-while >= a-seq)]
      (if (> (count a) (count b))
        (cons a (split-into-monotonics (drop-adj-while <= a-seq)))
        (cons b (split-into-monotonics (drop-adj-while >= a-seq)))))))

(defn permutations [a-set]
  (cond
   (< (count a-set) 2) '(()) ; Handle trivial cases
   (= (count a-set) 2) (rotations a-set)
   :else
   (let [rots (rotations a-set)]
     (apply concat ; Will give recursive lists if not concatenated
     (for [rot rots ; for all rotations..
           :let [a (first rot)]] ; ..take the first
       (for [perm (permutations (rest rot))] ; .. and recursively all permutations of the rest
         (cons a perm))))))) ; join the first and every permutation one by one

(defn powerset [a-set]
  (cond
   (empty? a-set) (set [(set [])]) ; Handle trivial cases
   :else
   (let [rots (rotations a-set)]
     (set
     (cons (set a-set)
     (apply concat ; Will give recursive lists if not concatenated
     (for [rot rots] ; for all rotations..
       (for [pows (powerset (rest rot))] ; .. drop the first and get all powersets there
         pows))))))))

