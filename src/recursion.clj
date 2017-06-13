(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

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
      (max (last a-seq)
       (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
 (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (last a-seq)
       (longest-sequence (rest a-seq))))))

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
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (= (pred? (first a-seq)) true)
     (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (= (pred? (first a-seq)) false)
      a-seq
   :else
     (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
 (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else
     (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (- up-to 1)
          (my-range (- up-to 1)))
    '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq
          (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
      (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (if (empty? a-map)
    a-map
    (let [[item cnt] (first a-map)]
      (concat (repeat cnt item) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq)  (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq))  (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
   (cond
    (<= (count a-seq) 1) a-seq
    :else
     (let [split (halve a-seq)
          left (first split)
          right (second split)]
       (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    (set a-seq)
    (let [monotonic? (fn [a-seq] (or
                                   (apply <= a-seq)
                                   (apply >= a-seq)))
          split-help (fn [prfix sufix]
                       (if (monotonic? prfix)
                         (list prfix sufix)
                         (recur (butlast prfix)
                                (cons (last prfix)
                                      sufix))))
          [prfix sufix] (split-help a-seq '())]
      (cons prfix (split-into-monotonics sufix)))))

(defn permutations [a-set]
  (let [ perm (fn [[ a & b] ]
                 (map (fn [c] (cons a c)) (permutations b)))]
    (cond
      (empty? a-set)
        (cons a-set a-set)
      (= 1 (count a-set))
        (list a-set)
      :else
        (apply concat (map perm (rotations a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (hash-set (hash-set))
    (clojure.set/union (powerset (next a-set))
           (map #(conj % (first a-set)) (powerset (next a-set))))))

