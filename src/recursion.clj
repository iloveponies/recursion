(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-length [seq]
  (if (empty? seq)
    0
    (+ 1 (seq-length (rest seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (seq-length seq-1) (seq-length seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
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
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (not= 0 (- (seq-length a-seq) (seq-length b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= n 0) 0
   :else (* n (power n (- k 1)))))


(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (tails a-seq)
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq))
     (my-frequencies-helper
       (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq))))
       (rest a-seq))
   :else (my-frequencies-helper
           (assoc freqs (first a-seq) 1)
           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (= 0 (count a-map))
    nil
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (empty? (rest a-seq)) (cons (first a-seq) '())
    :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn pred-seq [pred a-seq]
  (cond
   (empty? a-seq) true
   (empty? (rest a-seq)) true
   (pred (first a-seq) (second a-seq)) (pred-seq pred (rest a-seq))
   :else false))

(defn monotonic? [a-seq]
  (or (pred-seq <= a-seq) (pred-seq >= a-seq)))

(defn longseq [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longseq (rest a-seq)))))

(defn long-mono [a-seq]
  (longseq (filter monotonic? (inits a-seq))))

(defn remove-seq [a-seq from-seq]
  (if (empty? a-seq)
    from-seq
    (remove-seq (rest a-seq) (rest from-seq))))

(defn mono-helper [seq-set a-seq]
  (if (empty? a-seq)
    (reverse seq-set)
    (mono-helper (cons (long-mono a-seq) seq-set) (remove-seq (long-mono a-seq) a-seq))))

(defn split-into-monotonics [a-seq]
  (mono-helper '() a-seq))

(defn remove-elem-from-seq [acc elem a-seq]
  (cond
    (empty? a-seq) acc
    (= elem (first a-seq)) (concat acc (rest a-seq))
    :else (remove-elem-from-seq (cons (first a-seq) acc) elem (rest a-seq))))

(defn permuttelija [a-set acc]
  (let [helper (fn [x] (permuttelija (remove-elem-from-seq [] (nth a-set x) a-set) (conj acc (nth a-set x))))]
    (if (empty? a-set)
      acc
      (map helper (range 0 (count a-set))))))

(defn concat-x-times [a-set x]
  (if (zero? x)
    a-set
    (concat-x-times (apply concat a-set) (dec x))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (concat-x-times (permuttelija a-set []) (dec (count a-set)))))

(defn power-help [acc a-set]
  (let [union (fn [acc s1 s2] (cond
                                (and (empty? s1) (empty? s2)) acc
                                (empty? s1) (recur (conj acc (first s2)) nil (rest s2))
                                (empty? s2) (recur (conj acc (first s1)) (rest s1) nil)
                                :else (recur (conj acc (first s1) (first s2)) (rest s1) (rest s2))))
        conj-x (fn [x] (conj x (first a-set)))]
    (if (empty? a-set)
      acc
      (power-help (conj (union #{} acc (set (map conj-x acc))) #{(first a-set)}) (rest a-set)))))

(defn powerset [a-set]
  (power-help #{#{}} a-set))

