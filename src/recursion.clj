(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (max (first a-seq)
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (seq-max (first a-seq)
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    '()
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (count a-seq) (count b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
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
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs
                             (first a-seq)
                             (inc (get freqs (first a-seq) 0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce (fn [all [key val]]
            (concat all (repeat val key))) [] a-map))

(defn un-frequencies2 [a-map]
  (if (empty? (first a-map))
    '()
    (concat (repeat ((first a-map) 1) ((first a-map) 0))
            (un-frequencies2 (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (= (count a-seq) (count b-seq) 0) '()
   (= (count a-seq) 0) b-seq
   (= (count b-seq) 0) a-seq
   :else (let [sorted (if (< (first a-seq) (first b-seq))
                        {:min a-seq :high b-seq}
                        {:min b-seq :high a-seq})]
           (cons (first (sorted :min))
                 (seq-merge (rest (sorted :min)) (sorted :high))))))

(defn merge-sort [a-seq]
  (cond
   (= (count a-seq) 1) a-seq
   (= (count a-seq) 0) '()
   :else(seq-merge (merge-sort ((halve a-seq) 0))
                   (merge-sort ((halve a-seq) 1)))))

(defn split-into-monotonics2 [a-seq]
  (if (empty? a-seq)
    '(())
    (let [first (first a-seq)
          secnd (first (rest a-seq))]
      (if (< first secnd)
        (cons (seq first) (split-into-monotonics2 (rest a-seq)))
        (cons (seq first) (seq (split-into-monotonics2 (rest a-seq))))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono (last (take-while (fn [v]
                                   (or (< (count v) 2)
                                       (> (last v)
                                          (first (rest (reverse v))))))
                                 (inits a-seq)))
          rst (drop (count mono) a-seq)]
      (cons mono (split-into-monotonics rst)))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

(use 'clojure.repl)
(split-into-monotonics [0 1 2 1 0])  
(split-into-monotonics [0 5 4 7 1 3])
