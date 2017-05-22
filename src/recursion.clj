(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else
    false))

(defn my-last [coll]
  (first (reverse coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq)
         (if (empty? (rest a-seq))
           (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
         (if (empty? (rest a-seq))
           (first a-seq)
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
    (empty? a-seq)
      false
     (= elem (first a-seq))
       true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    []
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq )))
    :else
       []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      []
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
       a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
  (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (neg? (dec how-many-times)) '()
   :else
     (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) '()
   :else
     (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate-n [n a-seq]
  (if (< n (count a-seq))
    (cons a-seq
          (rotate-n (inc n) (concat (rest a-seq) (list (first a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (rotate-n 0 a-seq)))


(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freqs (if (nil? (get freqs elem))
                      (assoc-in freqs [elem] 1)
                      (update-in freqs [elem] inc))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (take n coll))

(defn my-drop [n coll]
  (drop n coll))

(defn halve [a-seq]
  (let [the-middle (int (/ (count a-seq) 2))]
    (into [] [(my-take the-middle a-seq) (my-drop the-middle a-seq)])))

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

