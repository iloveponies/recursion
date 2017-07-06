(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [comparable (fn [value] (if value value 0))]
  (if (empty?  a-seq)
    nil
    (max (first a-seq) (comparable (max-element (rest a-seq))))
  )))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [comparable (fn [value] (if value value []))]
    (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (comparable (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [value (first a-seq)]
      (if (pred? value)
        (cons value (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [value (first a-seq)]
      (if (pred? value)
        (cons value (my-take-while pred? (rest a-seq)))
        []))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (=
          (first a-seq)
          (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? (rest seq-1)) (empty? (rest seq-2)))
    (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons
      (f
        (first seq-1)
        (first seq-2)) '()))
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (zero? n)
    0
    (if (= 1 n)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    '()
    ))

(defn my-range [up-to]
  (if (< 0 up-to)
    (cons (- up-to 1) (my-range (- up-to 1)))
    '()
    ))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate [counter a-seq]
  (if (>= 1 counter)
    (cons a-seq '())
    (cons a-seq (rotate (- counter 1) (concat (rest a-seq) (cons (first a-seq) '()))))))

(defn rotations [a-seq]
  (rotate (count a-seq) a-seq)
)


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [repeat-element (fn [[elem value]]
                         (my-repeat value elem))]
    (if (empty? a-map)
      '(())
      (apply concat (map repeat-element a-map)))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (concat (cons (first coll) '()) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop
      (- n 1)
      (rest coll))))

(defn halve [a-seq]
  (let [first-halve-size (int (/ (count a-seq) 2))]
    (conj [(my-take first-halve-size a-seq)] (my-drop first-halve-size a-seq))))

(defn seq-merge-helper [merged a-seq b-seq]
  (if (empty? a-seq)
    (concat merged b-seq)
    (if (empty? b-seq)
      (concat merged a-seq)
      (if (< (first a-seq) (first b-seq))
        (seq-merge-helper (concat merged (cons (first a-seq) '())) (rest a-seq) b-seq)
        (seq-merge-helper (concat merged (cons (first b-seq) '())) a-seq (rest b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

