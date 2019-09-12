(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (= 1 (count coll)) true false))

(defn my-last [coll]
  (let [elements (count coll)]
  (cond
   (= 0 elements) nil
   (= 1 elements) (first coll)
   :else (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [elements (count a-seq)]
  (if (>= 1 elements)
    (first a-seq)
    (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (>= 1 (count a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

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
   (= elem (first a-seq)) true
   :else
   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   :else
   (if (pred? (first a-seq))
     (cons (first a-seq)
         (my-take-while pred? (rest a-seq)))
     '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (empty? b-seq) (not (empty? a-seq))) false
    (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) '()
   (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= 0 k) 1
   (= 1 k) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (>= 0 n) 0
   (= 1 n) 1
   (= 2 n) 1
   :else
   (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (>= 0 how-many-times) '()
   (= 1 how-many-times) [what-to-repeat]
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times)
                    what-to-repeat))))

(defn my-range [up-to]
  (cond
   (>= 0 up-to) '()
   (= 1 up-to) '(0)
   :else (cons (dec up-to)
               (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (>= 0 (count a-seq)) '(())
   :else
   (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map #(reverse %)
       (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) (list '())
  (take (count a-seq)
        (iterate #(concat (rest %) [(first %)])
                 (seq a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
      (my-frequencies-helper
       (assoc freqs
        (first a-seq)
         (if (contains? freqs (first a-seq))
           (inc (get freqs (first a-seq)))
           1))
       (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (if (empty? a-map) result
    (un-frequencies-helper
     (let [a (first a-map)]
       (concat result
               (repeat (second a) (first a))))
     (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll))
    '()
    (concat [(first coll)]
            (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take
            (- (count coll) n)
            (reverse coll))))

(defn halve [a-seq]
  (let [halve-point (int (/ (count a-seq) 2))]
    [(my-take halve-point a-seq)
     (my-drop halve-point a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [x (first a-seq) pred #(<= % x)]
    (if (empty? a-seq) b-seq
      (seq-merge (rest a-seq)
                 (concat (my-take-while
                          pred b-seq)
                         [x]
                         (my-drop-while
                          pred b-seq))))))


(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    (apply list a-seq)
    (let [halves (halve a-seq)]
      (seq-merge
       (merge-sort (first halves))
       (merge-sort (second halves))))))

(defn is-monotonic? [a-seq]
  (or (apply < a-seq)
      (apply > a-seq)))

(defn split-helper [result a-seq]
  (if (empty? a-seq) result
    (let [newseq
          (last (my-take-while is-monotonic?
               (rest (inits a-seq))))]
      (split-helper
       (conj result newseq)
       (drop (count newseq) a-seq)))))

(defn split-into-monotonics [a-seq]
  (seq (split-helper [] a-seq)))

(defn permutations [a-seq]
  (let [a-set (set a-seq)
        values (count a-set)]
    (cond
     (>= 0 values) (list '())
     (= 1 values) (list (list (first a-set)))
     (= 2 values)
          (list
           (list (first a-set) (second a-set))
           (list (second a-set) (first a-set)))
          (< 2 values)
           (apply concat (map
           (fn [value]
             (let [newset (disj a-set value)]
              (map #(conj % value)
                   (permutations newset))))
           a-set)))))

(defn powerset [a-seq]
  (let [a-set (set a-seq)]
  (if (empty? a-set) (set [a-set])
    (set (apply concat (set [a-set])
               (map (fn [value]
                       (powerset (disj a-set value)))
                    a-set))))))

