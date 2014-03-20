(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq)
              (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq)
                  (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
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
     '()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq) (empty? b-seq)
    (empty? b-seq) (empty? a-seq)
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) '()
    (empty? seq-2) '()
    :else (cons
            (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (neg? how-many-times) '()
   (zero? how-many-times) '()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (list* a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (list* a-seq) (inits (butlast a-seq)))))

(defn rotations-helper [a-seq rotations]
  (if (= 0 rotations)
    '()
    (cons (seq a-seq) (rotations-helper (cons (last a-seq) (drop-last a-seq)) (dec rotations)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
   '(())
   (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)
          new-frequencies (assoc freqs first-element (inc (get freqs first-element 0)))]
      (my-frequencies-helper new-frequencies (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(take half a-seq) (drop half a-seq)]))

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
   (empty? a-seq) a-seq
   (singleton? a-seq) a-seq
   :else (let [[seq1 seq2] (halve a-seq)]
           (seq-merge (merge-sort seq1)
                      (merge-sort seq2)))))

(defn monotonic? [a-seq]
  (or (empty? a-seq)
      (apply > a-seq)
      (apply < a-seq)))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [seq-inits (reverse (inits a-seq))
          seq-monotonics (take-while monotonic? seq-inits)
          largest (last seq-monotonics)
          remaining (drop (count largest) a-seq)]
      (cons largest (split-into-monotonics remaining)))))

(defn permutations [a-set]
  (if-not (seq a-set)
    (list ())
    (for [element a-set
          remaining-elements (permutations (for [remaining-element a-set
                                                 :when (not= remaining-element element)]
                                             remaining-element))]
      (cons element remaining-elements))))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [rest-set (powerset (rest a-set))
          first-and-rest-set (map #(conj % (first a-set)) rest-set)]
      (clojure.set/union rest-set first-and-rest-set))))