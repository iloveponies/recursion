(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll) (my-last (rest coll))))

; A little magic function
(defn magic [f a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
  (if (empty? xs)
    x
    (f x (magic f xs)))))

(defn max-element [a-seq]
  (magic max a-seq))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (magic seq-max a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    []
    (let [x (first a-seq)
          xs (rest a-seq)
          fxs (my-filter pred? xs)]
      (if (pred? x) (cons x fxs) fxs))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
     (empty? a-seq) '()
     (pred? x) (cons x (my-take-while pred? xs))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
     (empty? a-seq) '()
     (pred? x) (my-drop-while pred? xs)
     :else a-seq)))

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
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotation-helper [seq res n]
  (let [rotate (fn [seq] (concat (rest seq) [(first seq)]))
        rot-seq (rotate seq)]
    (if (zero? n)
      res
      (rotation-helper rot-seq (cons rot-seq res) (dec n)))))

(defn rotations [a-seq]
  (if (empty? a-seq) [[]]
  (rotation-helper a-seq [] (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)
        fx (inc (or (freqs x) 0))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs x fx) xs))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [x (first a-map)
       xs (rest a-map)]
  (if (empty? a-map)
    ()
    (concat (repeat (second x) (first x)) (un-frequencies xs)))))

(defn my-take [n coll]
  (cond
    (zero? n) ()
    (empty? coll) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (zero? n) coll
    (empty? coll) ()
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))
        seq1 (my-take n a-seq)
        seq2 (my-drop n a-seq)]
    [seq1, seq2]
  ))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        as (rest a-seq)
        b (first b-seq)
        bs (rest b-seq)]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (> a b) (cons b (seq-merge a-seq bs))
   :else (cons a (seq-merge as b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (singleton? a-seq) a-seq
   :else (let [[seq1 seq2] (halve a-seq)]
           (seq-merge (merge-sort seq1)
                      (merge-sort seq2)))))

; split-into-monotonics

(defn eat-monotone-rev [op a-seq current]
  (let [a (first a-seq)
        as (rest a-seq)]
    (cond
     (empty? a-seq) current
     (op (first current) a) (eat-monotone-rev op as (cons a current))
     :else current)))

(defn eat-monotone [op a-seq]
  "Read a subsequence of 'a-seq' that is monotonic under 'op'"
  (reverse (eat-monotone-rev op (rest a-seq) [(first a-seq)])))

(defn next-monotone [a-seq]
  "Greedily get the next monotone integer sequence"
  (let [inc-res (eat-monotone <= a-seq)
        dec-res (eat-monotone >= a-seq)]
        (if (< (count inc-res) (count dec-res))
          dec-res
          inc-res)))

(defn split-into-monotonics-helper [a-seq results]
  (if (empty? a-seq)
    results
    (let [res (next-monotone a-seq)
          new-seq (drop (count res) a-seq)]
      (split-into-monotonics-helper new-seq (concat results [res])))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper a-seq []))

; permutations

(defn permutations [a-set]
  [:-])

; powerset

; taken from the course material
(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn int-to-subset [k iset]
  (map second
       (filter (fn [x] (bit-test k (first x))) iset)))

(defn powerset [a-set]
  (let [i-set (indexed a-set)
        n (count a-set)
        indices (range 0 (power 2 n))]
    (map (fn [x] (int-to-subset x i-set)) indices)))
