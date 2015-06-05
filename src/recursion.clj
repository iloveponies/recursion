(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        true (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        true (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        true (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (and
         (= (first a-seq) (first b-seq))
         (= (empty? a-seq) (empty? b-seq))) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (map identity a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons
     (map identity a-seq)
     (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [x] (concat (my-drop-while #(not= x %) a-seq)
                         (my-take-while #(not= x %) a-seq)))
         a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)]
      (if (contains? freqs x)
        (my-frequencies-helper (assoc freqs x (inc (get freqs x))) (rest a-seq))        
        (my-frequencies-helper (assoc freqs x 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map #(repeat (get a-map %) %) (keys a-map))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    (apply list coll)
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (list (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) (apply list b-seq)
        (empty? b-seq) (apply list a-seq)
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [h (halve a-seq)]
      (seq-merge (merge-sort (first h)) (merge-sort (second h))))))

(defn first-monotonic [pred a-seq]
  (cond (empty? a-seq) '()
        (= (count a-seq) 1) (apply list a-seq)
        (pred (first a-seq) (second a-seq)) (cons (first a-seq) (first-monotonic pred (rest a-seq)))
        :else (list (first a-seq))))

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) '()
        (= (count a-seq) 1) (list (apply list a-seq))
        (<= (first a-seq) (second a-seq)) (let [m (first-monotonic <= a-seq)]
                                            (cons m (split-into-monotonics (drop (count m) a-seq))))
        :else (let [m (first-monotonic >= a-seq)]
                (cons m (split-into-monotonics (drop (count m) a-seq))))))

(defn permutations [a-set]
  (cond (empty? a-set) '(())
        (empty? (rest a-set)) (list (apply list a-set))
        :else (for [x a-set y (permutations (remove #{x} a-set))]
                (cons x y))))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [prev (powerset (rest a-set))]
      (concat (map #(cons (first a-set) %) prev) prev))))
