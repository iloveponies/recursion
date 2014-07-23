(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
   :else (max-element (cons (max (first a-seq) (second a-seq)) (rest (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (rest (rest a-seq))))
))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq) rst (rest a-seq)]
    (cond (empty? a-seq) a-seq
          (pred? fst) (cons fst (my-filter pred? rst))
          :else (my-filter pred? rst))))

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
   (not= (count a-seq) (count b-seq)) false 
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (first a-seq) (first b-seq)) false 
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (zero? k) 1
        :else     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (- n 2))) ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          cnt (inc (get freqs fst 0))]
      (my-frequencies-helper (assoc freqs fst cnt) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [unfreqs a-map]
  (if (empty? a-map)
    unfreqs
    (let [freq-pair (first a-map)
          seq (repeat (get freq-pair 1) (get freq-pair 0))]
      (un-frequencies-helper (concat unfreqs seq) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (empty? coll)
          (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll)
          (<= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [index (int (/ (count a-seq) 2))]
    (cons (take index a-seq) (cons (drop index a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else          (let [a (first a-seq) b (first b-seq)]
                    (if (<= a b)
                      (cons a (seq-merge (rest a-seq) b-seq))
                      (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

