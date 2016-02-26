(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [first (first a-seq)
          rest-max (max-element (rest a-seq))]
      (max first
           (if (nil? rest-max)
             first
             rest-max)))))

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
    (let [first-item (first a-seq)
          rest-items (rest a-seq)]
      (if (pred? first-item)
        (cons first-item (my-filter pred? rest-items))
        (my-filter pred? rest-items)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-item (first a-seq)]
      (if (pred? first-item)
        (cons first-item (my-take-while pred? (rest a-seq)))
        []))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-item (first a-seq)
          rest-items (rest a-seq)]
      (if (pred? first-item)
        (my-drop-while pred? (rest a-seq))
        a-seq))))

(defn seq= [a-seq b-seq]
  (if (= a-seq b-seq '[])
    true
    (if (and
          (not (empty? a-seq))
          (not (empty? b-seq))
          (= (first a-seq) (first b-seq)))
      (seq= (rest a-seq) (rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== 0 up-to)
    ()
    (let [dec-up-to (dec up-to)]
      (cons dec-up-to (my-range dec-up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (distinct (map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-item (first a-seq)
          freq (get freqs first-item)
          new-freqs (assoc freqs first-item
                      (if freq
                        (inc freq)
                        1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn my-un-frequencies-helper [tmp-seq a-map]
  (if (empty? a-map)
    tmp-seq
    (let [[elem n] (first a-map)]
      (my-un-frequencies-helper
        (concat tmp-seq (repeat n elem))
        (rest a-map)))))

(defn un-frequencies [a-map]
  (my-un-frequencies-helper () a-map))

(defn my-take [n coll]
  (if (or
        (empty? coll)
        (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [pivot (int (/ (count a-seq) 2))]
    [(my-take pivot a-seq)
     (my-drop pivot a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    ()
    (let [first-a (first a-seq)
          first-b (first b-seq)]
      (cond
        (nil? first-a)
          (cons first-b (seq-merge a-seq (rest b-seq)))
        (nil? first-b)
          (cons first-a (seq-merge (rest a-seq) b-seq))
        (< first-a first-b)
          (cons first-a (seq-merge (rest a-seq) b-seq))
        :else
          (cons first-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
