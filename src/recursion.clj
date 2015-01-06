(ns recursion)

(defn product [coll]
    (if (empty? coll)
        1
        (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
    (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
    (cond
        (empty? coll)      nil
        (singleton? coll) (first coll)
        :else             (my-last (rest coll))))

(defn max-element [a-seq]
    (cond
        (empty? a-seq)      nil
        (singleton? a-seq) (first a-seq)
        :else              (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
    (if (empty? a-seq)
        nil
        (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
    (cond
        (empty? a-seq)         ()
        (pred? (first a-seq))  (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else                  (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
    (cond
        (empty? a-seq)          false
        (= elem (first a-seq))  true
        :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
    (cond
        (empty? a-seq)        ()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else                 ()))

(defn my-drop-while [pred? a-seq]
    (cond
        (empty? a-seq)        ()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else                  a-seq))

(defn seq= [a-seq b-seq]
    (cond
        (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq)  (empty? b-seq)) false
        (= (first a-seq)    (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else                false))

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2))
        []
        (cons (f (first seq-1) (first seq-2))
              (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
    (if (= k 0)
        1
        (* n (power n (dec k)))))

(defn fib [n]
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
    (cond
        (< how-many-times 1) ()
        (< how-many-times 2) (seq [what-to-repeat])
        :else                (cons what-to-repeat
                                   (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
    (if (< up-to 1)
        ()
        (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
    (if (empty? a-seq)
        (seq [()])
        (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
    (if (empty? a-seq)
        [[]]
        (conj (inits (drop-last a-seq)) a-seq)))

(defn rotations [a-seq]
    (let [next-rotation (fn [x] (concat (rest x) [(first x)]))
          rotations-rec (fn rec [x n]
              (if (= n 0)
                  ()
                  (cons (seq x) (rec (next-rotation x) (dec n)))))
          len (count a-seq)]
        (if (= len 0)
            (seq [()])
            (rotations-rec a-seq len))))

(defn my-frequencies-helper [freqs a-seq]
    (let [a-key (first a-seq)
          freq (get freqs a-key)]
        (cond
            (empty? a-seq)           freqs
            (contains? freqs a-key) (my-frequencies-helper (assoc freqs a-key (inc freq)) (rest a-seq))
            :else                   (my-frequencies-helper (assoc freqs a-key 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
    (if (empty? a-map)
        ()
        (let [curr (first a-map)]
            (concat (repeat (val curr) (key curr))
                    (un-frequencies (rest a-map))))))

(defn my-take [n coll]
    (if (or (= n 0) (empty? coll))
        ()
        (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
    (if (or (= n 0) (empty? coll))
        coll
        (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
    (let [p (int (/ (count a-seq) 2))]
        [(my-take p (seq a-seq)) (my-drop p (seq a-seq))]))

(defn seq-merge [a-seq b-seq]
    (let [a (first a-seq)
          b (first b-seq)]
        (cond
            (empty? a-seq)  b-seq
            (empty? b-seq)  a-seq
            (< a b)        (cons a (seq-merge (rest a-seq) b-seq))
            :else          (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
    (cond
        (= (count a-seq) 0) ()
        (= (count a-seq) 1)  a-seq
        :else               (let [half (halve a-seq)]
                                (seq-merge (merge-sort (first half))
                                           (merge-sort (last half))))))

(defn monotonic? [x]
    (or (apply <= x) (apply >= x)))

(defn split-into-monotonics [a-seq]
    (if (< (count a-seq) 2)
        a-seq
        (let [curr (last (take-while monotonic? (rest (inits a-seq))))]
            (cons curr (split-into-monotonics (nthrest a-seq (count curr)))))))

(defn swap [a-seq i j]
    (let [a (get a-seq i)
          b (get a-seq j)]
        (assoc (assoc a-seq i b) j a)))

(defn find-first [f a-seq]
    (first (filter f a-seq)))

(defn next-permutation [a-seq]
    (let [k (find-first (fn [x] (< (get (vec a-seq) x)
                                   (get (vec a-seq) (inc x))))
                        (range (- (count a-seq) 2) -1 -1))]
        (if (nil? k)
            a-seq
            (let [l (find-first (fn [x] (< (get (vec a-seq) k)
                                           (get (vec a-seq) x)))
                                (range (dec (count a-seq)) -1 -1))
                  next-step (swap a-seq k l)]
                (concat (take (inc k) next-step)
                        (reverse (nthrest next-step (inc k))))))))

(defn find-permutations [a-seq]
    (let [next-step (next-permutation (vec a-seq))]
        (if (monotonic? next-step)
            (cons (seq a-seq) (seq [next-step]))
            (cons (seq a-seq) (find-permutations next-step)))))

(defn permutations [a-set]
    (if (empty? a-set)
        (seq [()])
        (find-permutations (sort a-set))))

(defn next-binary [x]
    (let [curr (last x)
          x-butlast (butlast x)]
        (cond
            (empty? x)    []
            (zero? curr) (concat x-butlast [1])
            :else        (concat (next-binary x-butlast) [0]))))

(defn get-bin [m a-set]
    (cond
        (empty? a-set)     #{}
        (zero? (first m)) (get-bin (rest m) (rest a-set))
        :else             (conj (get-bin (rest m) (rest a-set)) (first a-set))))

(defn powerset [a-set]
    (let [n (count a-set)
          find-powerset (fn find-p [m b-set]
              (if (every? zero? m)
                  #{#{}}
                  (conj (find-p (next-binary m) b-set)
                        (get-bin m b-set))))]
        (if (empty? a-set)
            #{#{}}
            (find-powerset (concat (repeat (dec n) 0) [1]) a-set))))

