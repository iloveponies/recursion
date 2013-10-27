(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not= nil (first coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
   (if (> (count seq-1) (count seq-2))
     seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) []
        :else (cons (f (first seq-1) (first seq-2))
                    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (== n 0) 0
        (== n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to) []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
  (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [number (fn [x] (if (= nil x) 0 x))]
    (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (assoc freqs (first a-seq) (+ 1 (number (get freqs (first a-seq)))))
     (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-freq-helper [un-freqs a-map]
  (let [freqs->list (fn [list] (repeat (get list 1) (get list 0)))]
    (if (empty? a-map)
      un-freqs
      (un-freq-helper (concat (freqs->list (first a-map)) un-freqs) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-freq-helper [] a-map))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
     [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [number (fn [x] (if (= nil x) 9999 x))]
    (if (and (empty? a-seq) (empty? b-seq))
      []
    (if (<= (number (first a-seq)) (number (first b-seq)))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn is-monotonic? [a-seq]
  (let [is? (fn [f-seq func] (loop [elem (first f-seq)
                                rest-seq (rest f-seq)]
                           (if (empty? rest-seq) true
                               (if (func elem (first rest-seq))
                                 (recur (first rest-seq) (rest rest-seq))
                                 false))))]
    (or (is? a-seq <=) (is? a-seq >=))))

(defn rev-inits [a-seq]
  (reverse (inits a-seq)))

(defn last-mon [x] (last (take-while is-monotonic? x)))

(defn remove-inits [x-seq]
  (for [f-seq (drop-while is-monotonic? x-seq)]
   (subvec (vec f-seq) (count (last-mon x-seq)))))

(defn split-into-monotonics [a-seq]
    (loop [inits-seq (rev-inits a-seq)
         acc []]
      (if (empty? inits-seq) acc
        (recur (remove-inits inits-seq)
               (conj acc (last-mon inits-seq))))))


(defn permutations [a-set]
  (if (>= 1 (count a-set)) [a-set]
    (loop [loop-set a-set
           acc []]
      (if (empty? loop-set) acc
        (recur (rest loop-set)
               (concat acc (map (fn [x-set] (cons (first loop-set) x-set))
                    (permutations (disj (set a-set) (first loop-set))))))))))


(defn powerset [a-set]
  (if (empty? a-set) [#{}]
    (concat (powerset (rest a-set))
            (map (fn [x-set] (conj x-set (first a-set)))
                 (powerset (rest a-set))))))

