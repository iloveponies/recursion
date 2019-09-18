(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll)
       (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll)
          (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq)
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) 
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) 
        a-seq
        (pred? (first a-seq)) 
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else []))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1
    (* (power n (dec k)) n)))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rest (map concat (tails a-seq) 
                      (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (inc (get freqs k 0))]
      (my-frequencies-helper (assoc freqs k v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [[k v] (first a-map)]
         (un-frequencies-helper (concat a-seq (repeat v k)) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (cond (<= n 0) []
        (empty? coll) []
        :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (empty? coll) []
        (pos? n) (my-drop (dec n) (rest coll))
        :else coll))

(defn halve [a-seq]
  (let [mid-idx (int (/ (count a-seq) 2))]
    [(my-take mid-idx a-seq)
     (my-drop mid-idx a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (pos? (compare (first a-seq) (first b-seq))) 
          (cons (first b-seq) 
                (seq-merge a-seq (rest b-seq)))
        :else (cons (first a-seq) 
                    (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[b-seq c-seq] (halve a-seq)]
      (seq-merge (merge-sort b-seq)
                 (merge-sort c-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [s (filter (complement empty?) (inits a-seq))
          m (first (filter #(or (apply < %) (apply > %)) s))]
      (cons m (split-into-monotonics (drop (count m) a-seq))))))

(defn permeate-value [value base]
  (let [size (count base)]
    (if (zero? size)
      [[value]]
      (map #(concat (take % base) [value] (drop % base)) 
           (range (inc size))))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (mapcat (partial permeate-value (first a-set))
            (permutations (rest a-set)))))

(defn powerset [a-set]
  (cond (empty? a-set) #{#{}}
        (singleton? a-set) #{#{} (set a-set)}
        :else (let [pset-rest (powerset (rest a-set))]
                (set (concat (map #(conj % (first a-set)) pset-rest)
                             pset-rest)))))
