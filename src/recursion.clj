(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [cur (first a-seq)
          nxt (max-element (rest a-seq))]
      (if (= nxt nil)
        cur
        (max cur nxt)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [cur (first a-seq)
          nxt (longest-sequence (rest a-seq))]
      (if (= nxt nil)
        cur
        (seq-max cur nxt)))))

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
    :else (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn all-but-last [a-seq]
  (reverse (rest (reverse a-seq))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (all-but-last a-seq)))))

(defn rotate [a-seq]
  (cons (last a-seq) (all-but-last a-seq)))

(defn rotations-with-counter[a-seq counter]
  (if (= counter 0)
    '()
    (cons a-seq (rotations-with-counter (rotate a-seq) (dec counter)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-with-counter a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-key (first a-seq)
          freqs-val (get freqs first-key)
          new-freqs-val (if (= freqs-val nil)
                          1
                          (inc freqs-val))]
      (my-frequencies-helper (assoc freqs first-key new-freqs-val) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [a-key (first (first a-map))
          a-val (second (first a-map))]
      (concat (repeat a-val a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0 )
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge-helper [a-seq b-seq merged]
  (cond (and (empty? a-seq) (not (empty? b-seq)))
          (concat merged b-seq)
        (and (empty? b-seq) (not (empty? a-seq)))
          (concat merged a-seq)
        :else
          (let [a-first (first a-seq)
                b-first (first b-seq)]
            (if (<= a-first b-first)
              (seq-merge-helper (rest a-seq) b-seq (concat merged [a-first]))
              (seq-merge-helper a-seq (rest b-seq) (concat merged [b-first]))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq '()))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [halved (halve a-seq)]
      (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
