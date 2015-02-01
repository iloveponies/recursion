(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))


(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (let [keep-big (fn [coll]
                   (if (< (first coll) (last coll))
                    (rest coll)
                    (pop (vec coll))))]
    (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
      :else (max-element (keep-big a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
          seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
        (= (seq-max (first a-seq) (last a-seq)) (last a-seq)) (longest-sequence (rest a-seq))
        :else (longest-sequence (butlast a-seq))))


(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not= (first a-seq) elem) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else (my-take-while pred? [])))


(defn my-drop-while [pred? a-seq]
    (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))


(defn seq= [a-seq b-seq]
  (cond
   (or (and (empty? a-seq) (not (empty? b-seq))) (and (empty? b-seq) (not (empty? a-seq)))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
    (and (not (empty? seq-1)) (not (empty? seq-2)))
         (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
  (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))


    (defn rotate [coll]
      (if (empty? coll)
        []
      (concat [(last coll)] (butlast coll))))



(defn rotations [a-seq]
  (let [rotate (fn [coll] (if (empty? coll)
        []
      (concat [(last coll)] (butlast coll))))]
  (cond
   (empty? a-seq) ['()]
   (and (seq? (first a-seq)) (= (count (first a-seq)) (count a-seq))) a-seq
   (seq? (first a-seq))
   (rotations (cons (rotate (first a-seq)) a-seq ))
   :else (rotations  [(rotate a-seq)]))))


(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

