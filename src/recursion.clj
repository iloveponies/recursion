(ns recursion)

(defn product [coll]
  (if
    (empty? coll)
     1
    (* (first coll) (product (rest coll)))))

(defn singleton? [a-coll]
  (let [rest-seq (rest a-coll)]
    (and (not (empty? a-coll)) (empty? rest-seq))))

(defn recurse-to-last [a-seq]
  (if
    (singleton? a-seq)
    (first a-seq)
    (recurse-to-last (rest a-seq))))

(defn my-last [a-seq]
  (if
    (empty? a-seq)
     nil
    (recurse-to-last a-seq)))

(defn recurse-max [a-seq]
  (if
    (singleton? a-seq)
    (first a-seq)
    (max (first a-seq) (recurse-max (rest a-seq)))))

(defn max-element [a-seq]
  (if
    (empty? a-seq)
     nil
    (recurse-max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if
    (> (count seq-1) (count seq-2))
     seq-1
     seq-2))

(defn recurse-longest-sequence [seq-of-seqs]
  (if
    (singleton? seq-of-seqs)
    (first seq-of-seqs)
    (seq-max (first seq-of-seqs)  (recurse-longest-sequence (rest seq-of-seqs)))))

(defn longest-sequence [seq-of-seqs]
  (if
    (empty? seq-of-seqs)
     nil
    (recurse-longest-sequence seq-of-seqs)))

(defn my-filter [pred a-seq]
  (if
    (empty? a-seq)
     a-seq
    (let [first-val (first a-seq)
          rest-vals (rest a-seq)]
    (if
      (pred first-val)
      (cons first-val (my-filter pred rest-vals))
      (my-filter pred rest-vals)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (==  elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred a-seq]
  (let [first-val (first a-seq)
        rest-vals (rest a-seq)]
  (cond
    (empty? a-seq) '()
    (pred first-val) (cons first-val (my-take-while pred (rest a-seq)))
    :else '())))

(defn my-drop-while [pred a-seq]
  (let [first-val (first a-seq)
        rest-vals (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred first-val) (my-drop-while pred (rest a-seq))
      :else a-seq
      )))

(defn seq= [seq-1 seq-2]
  (cond
    (not (= (empty? seq-1) (empty? seq-2))) false
    (and (empty? seq-1) (empty? seq-2)) true
    (and
      (= (first seq-1) (first seq-2))
      (seq= (rest seq-1) (rest seq-2))) true
    :else false))

(defn my-map [fun seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons
            (fun (first seq-1) (first seq-2))
            (my-map fun (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if
    (= 0 k)
     1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n ) 1
    :else (+
            (fib (- n 1))
            (fib (- n 2)))))

(defn my-repeat [times what]
  (cond
    (= 1 times) (cons what '())
    (< times 1) '()
    (< 1 times) (cons what (my-repeat (- times 1) what))))

(defn my-range [up-to]
  (cond
    (> 0 up-to) nil
    (= 0 up-to) '()
    (= 1 up-to) '(0)
    (< 1 up-to)
      (let [the-rest (my-range (- up-to 1))]
        (cons (+ 1 (first the-rest)) the-rest))))

(defn tails [a-vec]
  (if
    (empty? a-vec)
    '(())
    (cons (seq a-vec) (tails (rest a-vec)))))

(defn inits1 [a-vec]
  (if
    (empty? a-vec)
    '(())
    (cons (seq a-vec) (inits (reverse (rest (reverse a-vec)))))))

(defn inits [a-vec]
  reverse (inits1 a-vec))

(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))


(defn rotations [a-vec]
  (if
    (empty? a-vec)
    '()
    (distinct (map concat (tails a-vec) (reverse (inits a-vec))))))

(defn add-one-to-map-key [a-map a-key]
  (if (contains? a-map a-key)
    (assoc a-map a-key (+ 1 (get a-map a-key)))
    (assoc a-map a-key 1)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (add-one-to-map-key freqs (first a-seq)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-wrap-frequency [un-freqs value amount]
  (if (= 0 amount)
    un-freqs
    (un-wrap-frequency (cons value un-freqs ) value (- amount 1))))

(defn un-frequencies-helper [a-map un-freqs]
  (if (empty? a-map)
    un-freqs
    (let [value-amount-pair (first a-map)
          value (first value-amount-pair)
          amount (first (rest value-amount-pair))]
      (un-frequencies-helper (rest a-map) (un-wrap-frequency un-freqs value amount)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map {}))

(defn my-take-helper [n coll taken]
  (if (or (= n 0) (empty? coll))
    (reverse taken)
    (my-take-helper (- n 1) (rest coll) (cons (first coll) taken))))

(defn my-take [n coll]
  (my-take-helper n coll '()))

(defn my-drop-helper [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop-helper (- n 1) (rest coll))))

(defn my-drop [n coll]
  (my-drop-helper n coll))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))
        first-half (my-take midpoint a-seq)
        second-half (my-drop midpoint a-seq)]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (let [a (first a-seq)
            b (first b-seq)]
        (if (< a b)
          (cons a (seq-merge (rest a-seq) b-seq))
          (cons b (seq-merge a-seq (rest b-seq))))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)
          first-half (first halves)
          second-half (second halves)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

