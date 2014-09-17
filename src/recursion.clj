(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (empty? (rest coll)) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

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
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (cons (first a-seq) (my-drop-while (fn [x] false) (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons  (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [n k]
  (if (< n 1) '() (cons k (my-repeat (dec n) k))))

(defn my-range [n]
  (if (< n 1) '() (cons (dec n) (my-range (dec n)))))

(defn tails [a-seq]
  (if (empty? a-seq) (cons '() '()) (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) (cons '() '()) (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [n a-seq]
  (if (< n 1)
    '()
    (cons a-seq (rotations-helper
                 (dec n)
                 (concat
                  (rest a-seq)
                  (cons (first a-seq) '()))))))

(defn rotations [a-seq]
  (if (< (count a-seq) 1)
    (rotations-helper 1 a-seq)
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)]
      (my-frequencies-helper
       (assoc freqs elem
         (inc (if (contains? freqs elem) (get freqs elem) 0)))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [elem (first (keys a-map))]
    (concat (repeat (get a-map elem) elem)
     (un-frequencies (dissoc a-map elem))))))

(defn my-take [n coll]
  (if (< n 1)
    '()
    (if (<= (count coll) n) coll
      (cons (first coll)
            (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (or (< n 1) (< (count coll) 1))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (cons (my-take n a-seq) (cons (my-drop n a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (cond
   (= (count a-seq) (count b-seq) 0) '()
   (= (count a-seq) 0) b-seq
   (= (count b-seq) 0) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [])

(defn perm [a-seq, a-set]
  (if (empty? a-set)
    [a-seq]
    (for [elem a-set
          solution (perm (conj a-seq elem)
                         (disj a-set elem))]
      solution)))

(defn permutations [a-set] (perm () (set a-set)))

(defn powerset [a-seq]
  (loop [t a-seq p #{ #{} }]
    (if (empty? t) p
      (recur (rest t)
        (clojure.set/union p
          (map (fn [x] (conj x (first t))) p))))))

