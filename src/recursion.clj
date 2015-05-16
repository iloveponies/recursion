(ns recursion)

(defn product [coll]
  (if (empty? coll)
  1
  (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

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
      (if (> (first a-seq) (max-element (rest a-seq))) (first a-seq) 
            (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [size1 (count seq-1)
        size2 (count seq-2)]
        (if (> size1 size2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
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
    a-seq
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
    '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (not(pred? (first a-seq)))
    (cons (first a-seq) (rest a-seq))
    :else
    (my-drop-while pred? (rest a-seq))))
    

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
    true
    (or
    (or (empty? a-seq) (empty? b-seq))
    (not (= (first a-seq) (first b-seq)))) 
    false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
    '()
    :else
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? n)
    0
    (zero? k)
    1
    :else
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (<= how-many-times 0) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
  '()
  (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (= (count a-seq) 0)
  '(())
  (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [r-seq (reverse a-seq)]
    (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest r-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
  (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [ new-freqs (if (contains? freqs (first a-seq))
      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [n (first (rest (first a-map)))
        v (first (first a-map))]
  (if (empty? a-map)
  {}
  (concat (my-repeat n v) (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  (cond
    (< (count coll) n)
    coll
    (= n 0) '()
    :else
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
   (cond
    (< (count coll) n)
    '()
    (= n 0)
    coll
    :else
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
  (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
       b (first b-seq)]
    (cond
      (nil? a) b-seq
      (nil? b) a-seq
      :else (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (apply < a-seq) a-seq
    (> (count a-seq) 1)
      (let [halves (halve a-seq)]
      (seq-merge (merge-sort (get halves 0)) (merge-sort (get halves 1))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    (set a-seq)
    (let [mono? (fn [a-seq] (or
                                   (apply <= a-seq)
                                   (apply >= a-seq)))
          split (fn [a b]
                       (if (mono? a)
                         (list a b)
                         (recur (butlast a)
                                (cons (last a)
                                      b))))
          [a b] (split a-seq '())]
      (cons a (split-into-monotonics b)))))

(defn permutations [a-set]
  (seq
   (if (seq (rest a-set))
     (apply concat (for [x a-set]
                     (map #(cons x %) (permutations (remove #{x} a-set)))))
     [a-set])))

(defn powerset [a-set]
  (reduce (fn [a x]
            (->> a
                 (map #(set (concat #{x} %)))
                 (concat a)
                 set))
          #{#{}} a-set))

