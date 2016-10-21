(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else (my-last    (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                 []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
      (not= (count a-seq) (count b-seq)) false
      (and (empty? a-seq) (empty? b-seq))  true
      (not= (first a-seq) (first  b-seq))  false
      :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1)
        (empty? seq-2)) ()
    :else               (cons (f (first seq-1) (first seq-2))
                              (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (or (zero? n) (== n 1)) n
    :else   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [next (dec up-to)]
    (if (neg? next)
      ()
      (cons next (my-range next)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [tail (tails a-seq)
        init (inits a-seq)]
    (distinct (map concat tail init))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst-elem (first a-seq)
          pred-fst? (fn [x] (= fst-elem x))
          fst-elem-count (count (filter pred-fst? a-seq))
          new-freqs (assoc freqs fst-elem fst-elem-count)]
      (my-frequencies-helper new-freqs (remove pred-fst? a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [fst (first a-map)
          fst-elem (first fst)
          fst-count (second fst)
          repeated (repeat fst-count fst-elem)]
      (concat repeated (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (zero? n)     ()
    (empty? coll) ()
    :else         (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (zero? n)     coll
    (empty? coll) ()
    :else         (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (if (empty? a-seq)
    ()
    (cons (take half a-seq) (list (drop half a-seq))))))

(defn insert-in [elem a-seq]
  (cond
    (empty? a-seq)          (cons elem a-seq)
    (<= elem (first a-seq)) (cons elem a-seq)
    :else (cons (first a-seq) (insert-in elem (rest a-seq)))))

(defn seq-merge [a-seq b-seq]
  (let [a-fst (first a-seq)
        a-rest (rest a-seq)]
    (cond
      (or (empty? a-seq) (empty? b-seq)) (concat a-seq b-seq)
      :else (insert-in a-fst (seq-merge a-rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
      a-seq
      (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  (let [rep-fst (comp repeat first)
        perm-rest (comp permutations rest)
        partial-perms (fn [x] (map cons (rep-fst x) (perm-rest x)))]
    (if (empty? a-set)
      (list ())
      (mapcat partial-perms (rotations a-set)))))

(defn powerset [a-set]
  [:-])

