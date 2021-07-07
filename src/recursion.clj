(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (my-last (sort a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (if (= (first a-seq) (seq-max (first a-seq) (second a-seq)))
      (longest-sequence (vec (rest (assoc a-seq 1 (first a-seq)))))
      (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq))
      (cons
        (first a-seq)
        (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (== (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      []
    :else (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons
      what-to-repeat
      (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons
      (dec up-to)
      (my-range (dec up-to)))))

(defn tails [a-seq]
  (conj (map (fn [i] (reverse (vec (take i (reverse a-seq))))) (range (count a-seq))) a-seq))

(defn inits [a-seq]
  (conj (map #(vec (take % a-seq)) (range (count a-seq))) a-seq))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (map (fn [i]
           (let [prefixes (vec (inits a-seq))
                 suffixes (vec (tails a-seq))]
             (if (== i 0)
               a-seq
               (concat (get suffixes (inc i)) (get prefixes (- (count prefixes) i))))))
         (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-count (if (contains? freqs (first a-seq))
                      (inc (get freqs (first a-seq)))
                      1)]
      (my-frequencies-helper (assoc freqs (first a-seq) new-count) (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) []
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (== (count a-seq) 1))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (conj (split-into-monotonics (drop (count (take-while #(>= % (first a-seq)) a-seq)) a-seq))
          (take-while #(>= % (first a-seq)) a-seq))))

(defn permutations [a-set]
  (lazy-seq
   (if (seq (rest a-set))
     (apply concat (for [x a-set]
                     (map #(cons x %) (permutations (remove #{x} a-set)))))
     [a-set])))

(defn powerset [a-set]
  (if (empty? a-set) '(())
      (clojure.set/union (powerset (next a-set))
        (map #(conj % (first a-set)) (powerset (next a-set))))))

