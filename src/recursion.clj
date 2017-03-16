(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
      nil
      (if (singleton? a-seq)
          (first a-seq)
          (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
  (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq)
                (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
      false
      (or (= elem (first a-seq))
          (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
          (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))
          ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
      ()
      (if (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
          (cons (first a-seq)
                (my-drop-while (fn [x] false) (rest a-seq))))))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq)
          (empty? b-seq))
      (if (and (empty? a-seq)
               (empty? b-seq))
          true
          false)
      (and (= (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
      ()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
      1
      (* n (power n (- k 1)))))

(defn fib [n]
  (if (> 2 n)
      (if (= n 1)
          1
          0)
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
      ()
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
      ()
      (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      [[]]
      (conj (tails (into [] (rest a-seq))) a-seq)))

(defn inits [a-seq]
  (map (fn [seq] (into [] (reverse seq))) (tails (into [] (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
      [[]]
      (rest (my-map (fn [a b] (into [] (concat a b)))
                    (into [] (reverse (tails a-seq))) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs
                                    (first a-seq)
                                    (if (get freqs (first a-seq))
                                        (+ 1 (get freqs (first a-seq)))
                                        1))
                             (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
    (if (empty? a-map)
        ()
        (concat (let [item (first a-map)]
                (repeat (second item) (first item)))
                (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (= 0 n)
      ()
      (if (empty? coll)
          ()
          (concat (repeat 1 (first coll))
              (my-take (- n 1) (rest coll))))))

(defn my-drop [n coll]
  (if (>= 0 n)
      (if (empty? coll)
          ()
          (concat (repeat 1 (first coll))
                (my-drop (- n 1) (rest coll))))
      (my-drop (- n 1) (rest coll))))
      
          

(defn halve [a-seq]
  (let [c (count a-seq)
        a (int (/ c 2))]
  [(my-take a a-seq) (my-drop a a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq)
           (empty? b-seq))
      ()
      (if (empty? a-seq)
          (seq-merge b-seq a-seq)
          (if (empty? b-seq)
              (concat (repeat 1 (first a-seq))
                      (seq-merge (rest a-seq) b-seq))
              (if (<= (first a-seq) (first b-seq))
                  (concat (repeat 1 (first a-seq))
                          (seq-merge (rest a-seq) b-seq))
                  (seq-merge b-seq a-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
      a-seq
      (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
      true
      (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
      ()
      (let [mono (count (longest-sequence (filter monotonic? (inits a-seq))))]
        (concat [(my-take mono a-seq)]
                (split-into-monotonics (my-drop mono a-seq))))))

(defn permutations-helper [a-seq b-seq]
  (if (empty? a-seq)
      [b-seq]
      (apply concat (map (fn [x]
                             (permutations-helper (concat (my-take x a-seq) (my-drop (+ x 1) a-seq))
                                                  (concat [(nth a-seq x)] b-seq)))
                         (range (count a-seq))))))

(defn permutations [a-seq]
  (permutations-helper (into '() a-seq) []))

(defn list-filter [filters a-set]
  (if (empty? filters)
      ()
      (if (first filters)
          (concat [(first a-set)]
                  (list-filter (rest filters) (rest a-set)))
          (list-filter (rest filters) (rest a-set)))))

(defn exp-set [a-set step]
  (set (list-filter (map (fn [x] (if (< 0 (bit-and step (power 2 x)))
                                     true
                                     false))
                         (range 0 (count a-set)))
                    a-set))
)

(defn powerset-helper [a-set step]
  (if (= 0 step)
      #{#{}}
      (conj (powerset-helper a-set (- step 1))
            (exp-set a-set step))))

(defn powerset [a-set]
  (set (powerset-helper (set a-set) (power 2 (count a-set)))))

