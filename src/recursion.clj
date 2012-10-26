(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond 
   (singleton? coll) (first coll)
   (empty? coll) nil
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (my-last (sort a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [cur (first a-seq)]
    (cond 
     (singleton? a-seq) cur
     (empty? a-seq) nil
     :else (seq-max cur (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (let [cur (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? cur) (cons cur (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [cur (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? cur) (cons cur (my-take-while pred? (rest a-seq)))
     :else [])))

(defn my-drop-while [pred? a-seq]
  (let [cur (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? cur) (my-drop-while pred? (rest a-seq))
     :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    []))

(defn my-range [up-to]
  (if (== up-to 0)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (map reverse (tails (reverse a-seq)))))

(defn rotation-helper [n length a-seq]
  (if (== n length)
    []
    (cons a-seq
          (rotation-helper (inc n) length (cons (my-last a-seq) (butlast a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (seq '([]))
    (rotation-helper 0 (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          rst (rest a-seq)]
      (if (contains? freqs fst)
        (my-frequencies-helper (assoc freqs fst (inc (get freqs fst))) rst)
        (my-frequencies-helper (assoc freqs fst 1) rst)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [fst (first (first a-map))
          number (second (first a-map))
          rst (rest a-map)]
      (concat (repeat number fst) (un-frequencies rst)))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [length (count a-seq)
        elems (int (/ length 2))]
    (conj [(my-take elems a-seq)] (my-drop elems a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< a b) (cons a (seq-merge (rest a-seq) b-seq))
     :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [halved (halve a-seq)
        fst (first halved)
        snd (second halved)]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])