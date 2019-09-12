(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (let [r (rest coll)]
    (if (empty? r)
      (first coll)
      (my-last r))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst) (cons fst rst) rst))))

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
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn one? [n]
  (= n 1))

(defn fib [n]
  (cond
    (zero? n) 0
    (one? n) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

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
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (sequence a-seq) (inits (butlast a-seq)))))

(defn rotate [a-seq]
  (if (empty? a-seq)
    '()
    (cons (last a-seq) (butlast a-seq))))

(defn rotate-n [a-seq n]
  (if (zero? n)
    (sequence a-seq)
    (rotate-n (rotate a-seq) (dec n))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map #(rotate-n a-seq %) (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          value (get freqs elem)
          new-value (if value (inc value) 1)
          new-freqs (assoc freqs elem new-value)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if-not (empty? a-map)
     (let [[k v] (first a-map)
           part (repeat v k)]
       (concat part (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    (sequence coll)
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    (sequence b-seq)
    (if (empty? b-seq)
      (sequence a-seq)
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    (sequence a-seq)
    (let [[a b] (halve a-seq)
          sorted-a (merge-sort a)
          sorted-b (merge-sort b)]
      (seq-merge sorted-a sorted-b))))

(defn take-greedy-while [pred? a-seq]
  (first (filter #(apply pred? %) (inits a-seq))))

(defn get-with-length [len seqs]
  (if-not (empty? seqs)
    (let [fst (first seqs)]
      (if (= len (count fst)) fst (get-with-length len (rest seqs))))))

(defn get-longest [& seqs]
  (let [max-length (apply max (map count seqs))]
    (get-with-length max-length seqs)))

(defn split-into-monotonics [a-seq]
  (if-not (empty? a-seq)
    (let [asc (take-greedy-while < a-seq)
          desc (take-greedy-while > a-seq)
          const (take-greedy-while = a-seq)
          mono (get-longest asc desc const)
          res (my-drop (count mono) a-seq)]
      (cons mono (split-into-monotonics res)))))

(defn extract [a-seq]
  (map #(vector % (remove #{%} a-seq)) a-seq))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (apply concat (map (fn [[fst rst]]
           (map #(cons fst %) (permutations rst)))
         (extract a-seq)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [fst (first a-set)
          rst (rest a-set)
          b-set (powerset rst)
          c-set (set (map #(conj % fst) b-set))]
      (clojure.set/union b-set c-set))))

