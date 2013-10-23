(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (= (second coll) nil) (not (empty? coll))) true false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seqq-max [seq-1 seq-1-rest seq-2 seq-2-rest]
  (cond
    (empty? seq-1-rest) seq-2
    (empty? seq-2-rest) seq-1
    :else               (seqq-max seq-1 (rest seq-1-rest) seq-2 (rest seq-2-rest))))

(defn seq-max [seq-1 seq-2]
  (seqq-max seq-1 seq-1 seq-2 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [sequence-length (fn [len ns]
                             (if (empty? ns)
                               len
                               (recur (+ len 1) (rest ns))))]
      (let [lengths (map #(sequence-length 0 %) a-seq)]
        (get a-seq (.indexOf lengths (apply max lengths)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or  (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
    :else                               false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] ())
    (cons (map + a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn my-append [a-seq b]
  (cond
    (empty? a-seq) [b]
    :else          (cons (first a-seq) (my-append (rest a-seq) b))))

(defn rotations-helper [n a-seq]
  (if (= n (count a-seq))
    ()
    (cons a-seq (rotations-helper (inc n) (my-append (rest a-seq) (first a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '([])
    (rotations-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [freq (get freqs (first a-seq) 0)]
      (my-frequencies-helper (assoc freqs (first a-seq) (inc freq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [frt (first a-map)]
      (concat (repeat (second frt) (first frt)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq)                      b-seq
    (empty? b-seq)                      a-seq
    (< (first a-seq) (first b-seq))     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else                               (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a, b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn my-take-while [pred a-seq]
  (cond
   (empty? a-seq)       ()
   (pred (first a-seq)) (cons (first a-seq) (my-take-while pred (rest a-seq)))
   :else                ()))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq)     ()
   (singleton? a-seq) a-seq
   :else              (let [m (last (my-take-while monotonic? (reverse (inits a-seq))))]
                        (cons m (split-into-monotonics (my-drop (count m) a-seq))))))

(defn idx-exclude [a-set r]
  (vec (concat (take r a-set) (drop (inc r) a-set))))

(defn permutations-helper [acc-set a-set]
  (if (clojure.core/empty? a-set)
    acc-set
    (loop [n (dec (count a-set))
           ps ()]
      (if (neg? n)
        ps
        (recur (dec n) (concat ps (permutations-helper (conj acc-set (get a-set n)) (idx-exclude a-set n))))))))

(defn permutations [a-set]
  (partition (count a-set) (permutations-helper () a-set)))

(defn powerset [a-set]
  [:-])

