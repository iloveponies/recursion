  (ns recursion)

  (defn product [coll]
      (if (empty? coll)
      1
      (* (first coll)
         (product (rest coll)))))

  (defn singleton? [coll]
    (cond
     (empty? coll) false
     (empty? (rest coll)) true
     :else false))

  (defn my-last [coll]
      (if (or (empty? coll) (singleton? coll))
      (first coll)
         (my-last (rest coll))))

  (defn max-element [a-seq]
     (if (or (empty? a-seq) (singleton? a-seq))
      (first a-seq)
       (max (first a-seq)
         (max-element (rest a-seq)))))

  (defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2
      ))

  (defn longest-sequence [a-seq]
     (if (or (empty? a-seq) (singleton? a-seq))
      (first a-seq)
       (seq-max (first a-seq)
         (longest-sequence (rest a-seq)))))

  (defn my-filter [pred? a-seq]
     (if (empty? a-seq)
      a-seq
     (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

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
       ()
     (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else
       ()))

  (defn my-drop-while [pred? a-seq]
      (cond
        (empty? a-seq)
            ()
       (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
       :else
          a-seq))

  (defn seq= [a-seq b-seq]
     (cond
        (and (empty? a-seq) (empty? b-seq))
          true
        (not= (count a-seq) (count b-seq))
          false
        (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))
        :else
          false))

  (defn my-map [f seq-1 seq-2]
    (cond
        (or (empty? seq-1) (empty? seq-2))
          ()
        :else
           (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

  (defn power [n k]
    (if (zero? k)
    1
    (* n (power n (dec k)))))

  (defn fib [n]
    (cond
        (= 0 n)
         0
        (= 1 n)
         1
        :else
            (+ (fib (- n 1))
            (fib (- n 2)))))

  (defn my-repeat [how-many-times what-to-repeat]
    (if (<= how-many-times 0)
      ()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

  (defn my-range [up-to]
    (if (<= up-to 0)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))

  (defn tails [a-seq]
   (if (empty? a-seq)
   (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

  (defn inits [a-seq]
    (reverse (map reverse (tails (reverse a-seq)))))

  (defn rotations [a-seq]
  (if (< (count a-seq) 1)
    (cons () ())
    (concat (take (count a-seq) (partition (count a-seq) 1 (cycle a-seq))))))

 (defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          current-val (get freqs x 0)
          new-freqs (assoc freqs x (inc current-val))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

  (defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

  (defn un-frequencies [a-map]
    (if (empty? a-map)
      {}
    (let [[what times] (first a-map)
          ]
      (concat (repeat times what) (un-frequencies (rest a-map))))))

  (defn my-take [n coll]
    (if (or (empty? coll) (< n 1))
        ()
        (cons (first coll) (my-take (dec n) (rest coll)))))

  (defn my-drop [n coll]
     (cond
       (or (empty? coll) (> n (count coll)))
          ()
        (< n 1)
          coll
        :else
          (my-drop (dec n) (rest coll))))

  (defn halve [a-seq]
    (let [half (int (/ (count a-seq) 2))]
         (vector (take half a-seq) (drop half a-seq))))

  (defn seq-merge [a-seq b-seq]
    (cond
      (empty? b-seq)
        a-seq
      (empty? a-seq)
       b-seq
      (<= (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      :else
        (cons (first b-seq) (seq-merge  (rest b-seq) a-seq))
      ))

  (defn merge-sort [a-seq]
    (cond
      (empty? a-seq)
        ()
      (singleton? a-seq)
        a-seq
     :else
       (let [[a-half b-half] (halve a-seq)]
         (seq-merge (merge-sort a-half) (merge-sort b-half)))))

  (defn split-into-monotonics [a-seq]
    [:-])

  (defn permutations [a-set]
    [:-])

  (defn powerset [a-set]
    [:-])

