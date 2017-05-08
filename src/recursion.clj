(ns recursion)

(defn product [coll]
  ;; more concise
  ;;   (reduce (fn [a b] (* a b)) 1 coll))
  (if (empty? coll)
    1
    (let [a (first coll)
          rest (rest coll)]
      (if rest
        (* a (product rest))
        a))))

(defn singleton? [coll]
  ;; more complexity just for the sake of reduce
  ;; (simpler would be better)
  ;; (= 1 (reduce (fn [a b] (inc a)) 0 coll))
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  ;; using loop-recur
  ;; (loop [[val & rest] coll]
  ;;  (if rest
  ;;    (recur rest)
  ;;    val))
  (let [[a & rest] coll]
    (if rest
      (my-last rest)
      a)))

(defn max-element [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (max a (max-element rest))
      a)))

(defn seq-max [seq-1 seq-2]
  (first (sort-by count #(>= %1 %2) [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (seq-max a (longest-sequence rest))
      a)))

(defn my-filter [pred? a-seq]
  (into (empty a-seq)
        (let [a (first a-seq)
              rest (rest a-seq)]
          (if (empty? rest)
            (if (pred? a) (list a) '())
            (if (pred? a)
              (cons a (my-filter pred? rest))
              (my-filter pred? rest))))))

(defn sequence-contains? [elem a-seq]
  (let [a (first a-seq)
        rest (rest a-seq)]
    (if (empty? rest)
      (= a elem)
      (if (= a elem)
        true
        (sequence-contains? elem rest)))))

(defn my-take-while [pred? a-seq]
  (try
    (let [a (first a-seq)
             rest (rest a-seq)]
         (if (empty? rest)
           (if (pred? a) (list a) '())
           (if (pred? a)
             (if (pred? (first rest))
               (cons a (my-take-while pred? rest))
               (list a))
             '())))
    (catch Exception e '())))

(defn my-drop-while [pred? a-seq]
  ;; compositional solution
  ;;(keep-indexed
  ;;  (fn [a b] (when (> a (dec (count (my-take-while pred? a-seq)))) b))
  ;;  a-seq)

  ;; basic recursion
  (let [a (first a-seq)
        rest (rest a-seq)
        try-pred? #(try (pred? %) (catch Exception e false))]
    (if (empty? rest)
      (if (try-pred? a) '() (list a))
      (if (try-pred? a)
        (my-drop-while pred? rest)
        a-seq))))

(defn seq= [a-seq b-seq]
  (let [[a & a-rest] a-seq
        [b & b-rest] b-seq]
    (if (= (count a-seq) (count b-seq))
      (if (and (empty? a-rest)
               (empty? b-rest))
        (= a b)
        (if (= a b)
          (seq= a-rest b-rest)
          false))
      false)))

(defn my-map [f seq-1 seq-2]
  (let [[val-1 & rest-1] seq-1
        [val-2 & rest-2] seq-2]
    (if (or (not rest-1) (not rest-2))
      (if (not-any? nil? [val-1 val-2])
        (list (apply f [val-1 val-2]))
        '())
      (cons (apply f [val-1 val-2]) (my-map f rest-1 rest-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

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

