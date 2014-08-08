(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (->> coll (take 1) seq boolean) (empty? (rest coll))))

(defn my-last [[first & rest]]
  (if (empty? rest) first (my-last rest)))

(defn max-element [[x & xs]]
  (if (empty? xs)
    x
    (max-element (cons (max x (first xs)) (rest xs)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [[x & xs]]
  (if (empty? xs)
    x
    (longest-sequence (cons (seq-max x (first xs)) (rest xs)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

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
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (seq b-seq)) false
    (and (seq a-seq) (empty? b-seq)) false
    :else (if (= (first a-seq) (first b-seq))
            (seq= (rest a-seq) (rest b-seq))
            false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (== n 0) (== n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [coll]
  (for [n (->> coll count inc (range 0))]
    (drop n coll)))

(defn inits [coll]
  (for [n (->> coll count inc (range 0))]
    (take n coll)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map #(concat (drop % a-seq) (take % a-seq)) (-> a-seq count range))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (update-in (my-frequencies (rest a-seq)) [(first a-seq)] (fnil inc 0))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (dissoc a-map k))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [len (count a-seq)
        a (int (/ len 2))]
    [(my-take a a-seq) (my-drop a a-seq)]))

(defn seq-merge-helper [acc a-seq b-seq]
  (cond
    (empty? a-seq) (concat acc b-seq)
    (empty? b-seq) (concat acc a-seq)
    :else (let [a (first a-seq) b (first b-seq)]
      (if (< a b)
        (seq-merge-helper (concat acc [a]) (rest a-seq) b-seq)
        (seq-merge-helper (concat acc [b]) a-seq (rest b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper () a-seq b-seq))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[h1 h2] (halve a-seq)]
      (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn monotonic? [coll]
  (or (= coll (sort coll)) (= coll (sort > coll))))

(defn split-into-monotonics [coll]
  (if (empty? coll)
    ()
    (let [monotonic (->> coll inits (take-while monotonic?) last)]
      (cons monotonic (split-into-monotonics (drop (count monotonic) coll))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

