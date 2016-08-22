(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (let [eka (first coll)
        loput (rest coll)]
    (cond (empty? coll) false
          (empty? loput) true
          :else false)))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [s1 (count seq-1)
        s2 (count seq-2)]
    (if (> s1 s2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
  '()
  (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [eka (first a-seq)]
    (if (empty? a-seq)
    a-seq
    (if (pred? eka)
      (cons eka (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))))))

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
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) '()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times)
                    what-to-repeat))))

(defn my-range [up-to]
  (cond
   (< up-to 1) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq)
    ['()]
   :else
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [eka (first a-seq)
        apu (fn [x] (concat x [eka]))]
    (cond
     (empty? a-seq)
      '()
     :else
      (cons (seq a-seq)
            (map apu (seq (rotations (rest a-seq))))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [eka (first a-seq)] (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs eka)
                      (assoc freqs eka (inc (get freqs eka)))
                      (assoc freqs eka 1))]
      (my-frequencies-helper new-freqs (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [vektori (first a-map)
        [what how-many-times] vektori]
    (if (empty? a-map)
      (seq a-map)
      (concat (repeat how-many-times what)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll)
    '()
   (< n 1)
    coll
   :else
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [k (int (/ (count a-seq) 2))]
     [(my-take k a-seq) (my-drop k a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [eka1 (first a-seq)
        eka2 (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq))
      '()
     (empty? a-seq)
       (cons eka2 (seq-merge a-seq (rest b-seq)))
     (empty? b-seq)
       (cons eka1 (seq-merge (rest a-seq) b-seq))
     (<= eka1 eka2)
       (cons eka1 (seq-merge (rest a-seq) b-seq))
     :else
       (cons eka2 (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [n (count a-seq)
        [seq1 seq2] (halve a-seq)]
    (if (<= n 1)
      a-seq
      (seq-merge (merge-sort seq1)
                 (merge-sort seq2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

