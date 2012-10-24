(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? (rest coll)) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond 
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [longest 
        (fn this [seq-a seq-b]
          (cond
           (empty? seq-a) seq-2
           (empty? seq-b) seq-1
           :else (this (rest seq-a) (rest seq-b))))]
    (longest seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
   (empty? a-seq) '()
   (pred? (first a-seq)) (conj (my-take-while pred? (rest a-seq)) (first a-seq))
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
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (conj (my-map f (rest seq-1) (rest seq-2)) (f (first seq-1) (first seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1 ) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (conj (tails (rest a-seq)) (seq a-seq))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
  (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [head (first a-seq)
        rec-freqs my-frequencies-helper
        tail (rest a-seq)]
  	(cond
     (empty? a-seq)
     freqs
     (contains? freqs head)
     (rec-freqs (assoc freqs head (inc (get freqs head))) tail)
     :else
     (rec-freqs (assoc freqs head 1) tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (get (first a-map ) 1) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (= n 0)
   '()
   (empty? coll)
   '()
   :else
   (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [merge-help 
        (fn this [a-seq b-seq m-seq]
          (cond
           (and (empty? a-seq) (empty? b-seq))
           m-seq
           (empty? a-seq)
           (this a-seq (rest b-seq) (conj m-seq (first b-seq)))
           (empty? b-seq)
           (this (rest a-seq) b-seq (conj m-seq (first a-seq)))
           (< (first a-seq) (first b-seq))
           (this (rest a-seq) b-seq (conj m-seq (first a-seq)))
           :else
           (this a-seq (rest b-seq) (conj m-seq (first b-seq)))))]
  (seq (merge-help a-seq b-seq []))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (let [splitter 
        (fn this [a-seq res-seq n k]
          (cond
           (>= n (count a-seq))
           (conj res-seq (take n a-seq))
           (>= (nth a-seq n) k)
           (this a-seq res-seq (inc n) (nth a-seq n))
           :else
            (this (drop n a-seq) (conj res-seq (take n a-seq))
                                       1 (apply min a-seq))))]
    (reverse (splitter a-seq '() 1 (apply min a-seq)))))

(defn permutations [a-set] ;ei ehka kaunis, mutta toimii :D
  (let [insert 
        (fn [a-seq elem n] 
          (concat (take n a-seq) (conj (drop n a-seq) elem)))
        insertions
        (fn this [a-seq elem n]
          (if (= n (count a-seq))
            (conj '() (insert a-seq elem n))
            (conj (this a-seq elem (inc n))
                  (insert a-seq elem n))))
        perm 
        (fn this [a-set perms]
               (if (empty? a-set)
                 perms
                 (this (rest a-set) 
                      (apply concat 
                             (map 
                              (fn [a-seq] 
                                (insertions a-seq (first a-set) 0)) perms)))))]
    (perm a-set '(()) )))

(defn powerset [a-set] ;samalla logiikalla :D
  (let [insert 
        (fn [a-seq elem n] 
          (concat (take n a-seq) (conj (drop n a-seq) elem)))
        insertions
        (fn this [a-seq elem n]
          (if (= n (count a-seq))
            (conj '() (insert a-seq elem n))
            (conj (this a-seq elem (inc n))
                  (insert a-seq elem n))))
        pow
        (fn this [a-set perms]
          (if (empty? a-set)
            perms
            (this (rest a-set)
                 (concat perms
                         (apply concat (map (fn [a-seq]
                                              (insertions a-seq (first a-set) 0))
                                            perms))))))]
    (set (map set (pow a-set '(()) )))))