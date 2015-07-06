(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond (singleton? a-seq) (first a-seq)
        (empty? a-seq) nil
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (singleton? a-seq) (first a-seq)
        (empty? a-seq) nil
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
    (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ' ()
      (if (pred? (first a-seq))
          (cons (first a-seq) (my-take-while pred? (rest a-seq)))
          ())))

(defn my-drop-while [pred? a-seq]
    (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (== (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) ()
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (== n 0) 0
        (== n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
      ()
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      (cons a-seq ())
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (if (empty? a-seq)
      (cons a-seq ())
      (cons (reverse a-seq) (tails (rest (reverse a-seq)))))))

(defn rotation-helper [a-seq n]
  (let [one-rot (concat (rest a-seq) [(first a-seq)])]
    (cond (empty? a-seq) '(())
          (<= n 0) ()
          :else (cons one-rot (rotation-helper one-rot (dec n)) ))))

(defn rotations [a-seq]
  (rotation-helper a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
      freqs
      (let [elem (first a-seq)
            frequency-count (if (contains? freqs elem)
                                    (assoc freqs elem (inc (freqs elem)))
                                    (assoc freqs elem 1))]

        (my-frequencies-helper frequency-count (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (let [repeats (first (vals a-map))
        repeatable (first (keys a-map))
        ]
    (if (empty? a-map)
        result
        (un-frequencies-helper (concat result (repeat repeats repeatable)) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
      []
      (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
      coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) ()
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (let [half-point (int (/ (count a-seq) 2))]
  (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort (my-take half-point a-seq)) (merge-sort(my-drop half-point a-seq))))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

