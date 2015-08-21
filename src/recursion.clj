(ns recursion)

(defn product [coll]
  (if(empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (not (or
         (empty? coll)
         (seq (rest coll)))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? (rest a-seq))
    (if (pred? (first a-seq)) [(first a-seq)] '())
    (concat (if (pred? (first a-seq)) [(first a-seq)] '())
            (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (or (= (first a-seq) elem) ;will short circuit
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) []
    (if (pred? (first a-seq))
      (concat [(first a-seq)] (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) []
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq) (empty? b-seq)
    (empty? b-seq) false
    :else (and (= (first a-seq) (first b-seq))
               (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) []
    (concat [(f (first seq-1) (first seq-2))]
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) []
    (conj (my-repeat (- how-many-times 1) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (>= 0 up-to) []
    (concat [(- up-to 1)] (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) [[]]
    (concat [a-seq] (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (set (map concat (reverse (tails a-seq)) (inits a-seq))))

(defn my-frequencies-helper [freqs a-seq]
   (if (empty? a-seq) freqs
     (my-frequencies-helper (assoc freqs (first a-seq)
                                   (+ 1 (get freqs (first a-seq) 0)))
                            (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
    (concat (repeat (second (first a-map)) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n)) []
    (concat [(first coll)] (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= 0 n)) coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    [(my-take h a-seq) (my-drop h a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
    :else (concat [(first b-seq)] (seq-merge (rest b-seq) a-seq))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq)) a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics-helper [cur a-seq history]
  (if (empty? a-seq)
    (if (empty? cur) history (conj history cur))
    (if (monotonic? (conj cur (first a-seq)))
      (split-into-monotonics-helper (conj cur (first a-seq))
                                    (rest a-seq) history)
      (split-into-monotonics-helper [] a-seq (conj history cur)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper [] a-seq []))

(defn permutations-set [a-set]
  (cond
    (empty? a-set) [[]]
    (empty? (rest a-set)) [[(first a-set)]]
    :else (apply concat (map
            (fn [i] (map
                      #(concat [i] %)
                      (permutations-set (disj a-set i))))
            a-set))))

(defn permutations [a-set]
  (permutations-set (set a-set)))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
    (let [p (powerset (set (rest a-set)))]
      (clojure.set/union p (set (map #(conj % (first a-set)) p))))))
