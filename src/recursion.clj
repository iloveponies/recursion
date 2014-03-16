(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :default (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :default (max (first a-seq) (max-element (rest a-seq)))))



(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? f) (cons f (my-filter pred? r))
      :default (my-filter pred? r))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? f) (cons f (my-take-while pred? (rest a-seq)))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
    (if (< up-to 1)
      '()
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (concat (tails (rest a-seq)) [a-seq] )))

(defn inits [a-seq]
  (map (fn [x] (reverse x)) (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [len (count a-seq)
        a-list (list* a-seq)
        my-rotate (fn rot [rotseq n] (if (< n 1) rotseq (rot (cons (last rotseq) (drop-last 1 rotseq)) (dec n))))
        my-rot (fn [n] (my-rotate a-list n))
        rotrng (range len)]
    (cond
      (empty? a-seq) (list '())
      (singleton? a-seq) a-seq
      :default (map my-rot rotrng))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          rst (rest a-seq)
          counter (if (contains? freqs fst) (inc (get freqs fst)) 1)]
      (my-frequencies-helper (assoc freqs fst counter) rst))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (flatten (concat (map (fn [[key num]] (repeat num key)) a-map))))

(defn my-take [n coll]
  (let [len (count coll)]
    (cond
      (>= n len) coll
      (empty? coll) coll
      :default (my-take n (drop-last 1 coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [len (count a-seq)
        half (int (/ len 2))]
    (if (empty? a-seq)
      a-seq
      (vector (my-take half a-seq) (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (let [a-fst (first a-seq)
        b-fst (first b-seq)
        a-rst (rest a-seq)
        a-smaller (fn [x] (< x b-fst))
        b-smaller (fn [x] (< x a-fst))]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a-fst b-fst) (seq-merge (my-drop-while a-smaller a-seq) (cons a-fst b-seq))
      :default (seq-merge
                 a-rst
                 (concat
                   (my-take-while b-smaller b-seq)
                   (cons a-fst (my-drop-while b-smaller b-seq)))))))


(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

