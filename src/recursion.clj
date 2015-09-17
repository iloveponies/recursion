(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (= (rest coll) [])))

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
      (max (first a-seq)
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
      (seq-max (first a-seq)
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
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [value (dec up-to)]
    (if (< up-to 1)
      []
      (cons value (my-range value)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (cons a-seq (inits (drop-last a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (drop-last (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [[seq-fst] a-seq]
    (if (empty? a-seq)
      freqs
      (if (contains? freqs seq-fst)
        (my-frequencies-helper
         (assoc freqs seq-fst (inc (get freqs seq-fst))) (rest a-seq))
        (my-frequencies-helper
         (assoc freqs seq-fst 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[fst-amount] (vals a-map)
        [fst-key] (keys a-map)]
    (if (empty? a-map)
      []
      (concat (repeat fst-amount fst-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (empty? coll)
    []
    (if (> n 0)
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (empty? coll)
    []
    (if (> n 0)
      (concat
       (my-drop (dec n) (drop-last coll))
       [(last coll)]))))

(defn halve [a-seq]
  (let [length (count a-seq)
        fst-half (int (/ length 2))
        sec-half (- length fst-half)]
    (if (empty? a-seq)
      []
      (if (= fst-half 0)
        (vector [] (my-drop sec-half a-seq))
        (vector (my-take fst-half a-seq) (my-drop sec-half a-seq))))))


(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    []
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= (first a-seq) (first b-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else
       (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [divided-seq (halve a-seq)
          seq-1 (first divided-seq)
          seq-2 (second divided-seq)]
      (seq-merge (merge-sort seq-1) (merge-sort seq-2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

