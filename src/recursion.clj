(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
       (empty? (rest coll))
    ))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (apply max a-seq)
  ))


(defn seq-max [seq-1 seq-2]
  (let [count1 (count seq-1)
       count2 (count seq-2)]
    (cond (> count1 count2) seq-1
          (<= count1 count2) seq-2
          :else nil)))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
  (longest-sequence (seq-max (first a-seq) (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [front (first a-seq)
        back (rest a-seq)]
  (cond (empty? a-seq) ()
        (pred? front) (cons front (my-filter pred? back))
    :else (my-filter pred? back))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
      (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [front (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? front) (cons front (my-take-while pred? (rest a-seq)))
    :else ())))

(defn my-drop-while [pred? a-seq]
  (let [front (first a-seq)
        back (rest a-seq)]
  (cond
    (empty? a-seq) a-seq
    (pred? front) (my-drop-while pred? back)
    :else a-seq)))

(defn seq= [a-seq b-seq]
  (= a-seq b-seq))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (let [value (f (first seq-1) (first seq-2))]
    (cons value (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (zero? k) 1
  (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if(= up-to 0) ()
  (cons (dec up-to )(my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      (cons a-seq ())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
    (set (map concat (tails a-seq) (inits a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
      (let [head (first a-seq)
            freq (get freqs head)
            value (if freq freq 0)]
        (my-frequencies-helper
          (assoc freqs head (inc value))
          (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
   (if (empty? a-map)
     (empty ())
     (let [entry (first a-map)
          my-key (first entry)
          my-value (second entry)]
     (concat (repeat my-value my-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (> 1 n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int  (/ (count a-seq) 2))]
   (concat [(my-take half a-seq)] [(my-drop half a-seq)])))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      :else (let [head-a (first a-seq)
                  head-b (first b-seq)]
              (if (< head-a head-b)
                (cons head-a (seq-merge (rest a-seq) b-seq))
                (cons head-b (seq-merge a-seq (rest b-seq) ))))))

(defn one-or-less? [coll]
  (or (empty? coll) (empty? (rest coll))))

(defn merge-sort [a-seq]
  (if (one-or-less? a-seq)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

