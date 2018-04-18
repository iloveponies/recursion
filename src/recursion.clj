(ns recursion)

(defn product [coll]
      (if (empty? coll)
        1
        (*
          (first coll)
          (product (rest coll)))))

(defn singleton? [coll]
      (and
        (not (empty? coll))
        (empty? (rest coll))))

(defn my-last [coll]
      (if (empty? coll)
        nil
        (if (singleton? coll)
          (first coll)
          (my-last (rest coll)))))


(defn max-element [a-seq]
      (if (or (singleton? a-seq) (empty? a-seq))
        (first a-seq)
        (max
          (first a-seq)
          (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
      (if (> (count seq-1) (count seq-2))
        seq-1
        seq-2))

(defn longest-sequence [a-seq]
      (if (or (singleton? a-seq) (empty? a-seq))
        (first a-seq)
        (seq-max
          (first a-seq)
          (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
      (if (empty? a-seq)
        a-seq
        (if (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
      (cond
        (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
      (cond
        (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
      (cond
        (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq
        ))

(defn seq= [a-seq b-seq]
      (cond
        (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        :else (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
      (cond
        (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2))


                    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
      (if (zero? k)
        1
        (* n (power n (dec k)))))

(defn fib [n]
      (cond
        (= n 0) 0
        (= n 1) 1
        :else (+
                (fib (- n 1))
                (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
      (cond
        (> how-many-times 0) (cons
                               what-to-repeat
                               (my-repeat
                                 (- how-many-times 1)
                                 what-to-repeat))
        :else ()))

(defn my-range [up-to]
      (if (> up-to 0)
        (cons (- up-to 1) (my-range (- up-to 1)))
        ()))

(defn tails [a-seq]
      (cond
        (= (count a-seq) 0) '(())
        :else (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
      (cond
        (= (count a-seq) 0) '(())
        :else (cons
                (seq a-seq)
                (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
      (if (empty? a-seq)
        '(())
        (rest (map concat (tails a-seq) (reverse (inits a-seq))))))


(defn my-frequencies-helper [freqs a-seq]
      (if (empty? a-seq)
        freqs
        (let [curr-elem (first a-seq)
              current-count (get freqs curr-elem 0)
              new-count (inc current-count)
              updated-freqs-map (assoc freqs curr-elem new-count)]
             (my-frequencies-helper updated-freqs-map (rest a-seq)))))

(defn my-frequencies [a-seq]
      (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
      (let [current-elem (first (keys a-map))
            current-elem-cnt (first (vals a-map))
            ]
           (if (> (count a-map) 0)
             (concat (my-repeat current-elem-cnt current-elem) (un-frequencies (dissoc a-map current-elem)))
             )))


(defn my-take [n coll]
      (let [curr-elem (first coll)
            new-n (dec n)]
           (if (and (not (empty? coll)) (> n 0))
             (cons curr-elem (my-take new-n (rest coll))))))

(defn my-drop [n coll]
      (cond
        (> n (count coll)) '()
        (> n 0) (my-drop (dec n) (rest coll))
        :else (seq coll)))

(defn halve [a-seq]
      (let [part-1-size (int (/ (count a-seq) 2))]
           (if (> (count a-seq) 1)
             [(my-take part-1-size a-seq) (my-drop part-1-size a-seq)]
             [ '() a-seq])))

(defn seq-merge [a-seq b-seq]
      (let [size-a (count a-seq)
            size-b (count b-seq)
            first-a (first a-seq)
            first-b (first b-seq)
            rest-a (rest a-seq)
            rest-b (rest b-seq)]
           (cond
             (and (= size-a 0)(= size-b 0)) '()
             (= size-a 0) (seq b-seq)
             (= size-b 0) (seq a-seq)
             (< first-a first-b) (cons first-a (seq-merge rest-a b-seq))
             (> first-a first-b) (cons first-b (seq-merge a-seq rest-b)))))

(defn merge-sort [a-seq]
      (if (> (count a-seq) 1)
        (apply seq-merge (map merge-sort (halve a-seq)))
        (seq a-seq)))

(defn split-into-monotonics [a-seq]
      [:-])

(defn permutations [a-set]
      ())

(defn powerset [a-set]
      [:-])

