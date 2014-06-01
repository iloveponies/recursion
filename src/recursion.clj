(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) ()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (and (empty? a-seq) (not (empty? b-seq))) (and (empty? b-seq) (not (empty? a-seq)))) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (letfn [(loop [a-seq acc rots]
              (if (empty? a-seq)
                rots
                (loop ;!!!!!!!!!!!!!!
                  (rest a-seq)
                  (cons (first a-seq) acc)
                  (cons (concat a-seq (reverse acc)) rots)
                  )))]
    (loop a-seq nil nil))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          freq (if (freqs elem) (freqs elem) 0)
          new-freqs (conj freqs {elem (inc freq)})]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [pr] (repeat (second pr) (first pr))) a-map)))

(defn my-take [n coll]
  (letfn [(take-helper [x acc coll]
                       (if (or (empty? coll) (<= x 0))
                         (reverse acc);!!!!!!!!!!!!!!
                         (take-helper (dec x) (cons (first coll) acc ) (rest coll))))]
    (take-helper n nil coll)))

(defn my-drop [n coll]
  (letfn [(drop-helper [x coll]
                       (cond (empty? coll) ()
                             (<= x 0) coll
                             :else (drop-helper (dec x) (rest coll))))]
    (drop-helper n coll)))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [ (my-take n a-seq) (my-drop n a-seq) ] ))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq) )
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)) )))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [monots (take-while (fn [s] (or (apply <= s) (apply >= s))) (rest (reverse (inits a-seq))))
          monot-seq (last monots)
          the-rest (my-drop (count monots) a-seq)]
    (cons monot-seq (split-into-monotonics the-rest)))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    [a-set]
    (let [perms (permutations (rest a-set))
          elem (first a-set)
          include-elem (fn [perm]
                         (let [p-inits (reverse (inits perm))
                               p-tails (tails perm)
                               zipped (map vector p-inits p-tails)]
                           (map (fn [[a b]] (concat a [elem] b)) zipped)))]
      (apply concat (map include-elem perms)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{ #{} }
    (let [first-elem (first a-set)
          without-first (powerset (set (rest a-set)))
          with-first (set (map (fn [s] (conj s first-elem)) without-first))]
      (clojure.set/union with-first without-first))))
