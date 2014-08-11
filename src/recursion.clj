(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) 
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) 
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1) 
        c2 (count seq-2)]
    (if (== (max c1 c2) c1) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq)
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (false? (pred? (first a-seq))) '()
        :else (cons (first a-seq) 
                    (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (false? (pred? (first a-seq))) (seq a-seq)
        :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
      (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) n
      (+ (fib (dec n)) 
         (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
      (cons what-to-repeat 
            (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (rest (map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-freqs-helper [freqs a-seq]
  (let [a-key (first a-seq)
        a-val (get freqs a-key)] 
    (cond (empty? a-seq) freqs 
          (contains? (set (keys freqs)) a-key) (my-freqs-helper 
                                                (assoc freqs a-key (inc a-val)) 
                                                (rest a-seq))
          :else (my-freqs-helper 
                 (assoc freqs a-key 1) 
                 (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-freqs-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
      (concat (un-frequencies (rest a-map))
              (repeat (val (first a-map)) 
                      (key (first a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1)) '()
      (cons (first coll) 
            (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (< n 1) coll
        (> n (count coll)) '()
        :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq), b (first b-seq)]
    (cond (empty? a-seq) b-seq
          (empty? b-seq) a-seq
          (= (min a b) a) (cons a (seq-merge (rest a-seq) b-seq))
          :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [halved (halve a-seq)]
    (cond (empty? a-seq) '()
          (singleton? a-seq) a-seq
          :else (seq-merge (merge-sort (first halved))
                           (merge-sort (second halved))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

