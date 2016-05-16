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
  (let [first-one (first coll)
        the-others (rest coll)]
    (if (empty? the-others)
      first-one
      (my-last the-others))))

(defn max-element [a-seq]
  (let [[x & z] a-seq]
    (if (empty? z)
     x
      (max x (max-element z)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (<= c1 c2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)
          pred-x (pred? x)
          z (rest a-seq)
          filterz (my-filter pred? z)]
      (if pred-x
        (cons x filterz)
        filterz))))

(defn sequence-contains? [elem a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
      (empty? a-seq) false
      (= elem x)true
      :else (sequence-contains? elem xs))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [x (first a-seq)
          pred-x (pred? x)
          z (rest a-seq)
          do-rest (my-take-while pred? z)]
      (cond
        pred-x (cons x do-rest)
        :else '()))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or  (empty? a-seq) (empty? b-seq)) false
    :else (and
          (== (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
     1
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 1 n) 1
    (= 0 n) 0
    :else (+
          (fib (dec n))
          (fib (dec (dec n))))))

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
    (seq ['()])
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse
    (map
      reverse
        (tails
          (reverse a-seq)))))


(defn rotations [a-seq]
  (let [result (rest (map concat (tails a-seq) (inits a-seq)))]
    (if (empty? result)
      (seq ['()])
        result)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          current-val (get freqs x 0)
          newfq (assoc freqs x (inc current-val))]
      (my-frequencies-helper newfq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (seq [])
      (let [[x z] (first a-map)]
        (concat (repeat z x) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [divide (int (/ (count a-seq) 2))]
    [(my-take divide a-seq) (my-drop divide a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge  a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (singleton? a-seq) a-seq
    :else (let [[h1 h2] (halve a-seq)]
           (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn pair-check [pred x]
  (cond
   (empty? x) true
   (singleton? x) true
   :else (and (pred (first x) (second x)) (pair-check pred (rest x)))))

(defn is-monotonic? [x]
  (or (pair-check <= x) (pair-check >= x)))

(defn split-into-monotonics [a-seq]
  (let [a-inits (inits a-seq)
        monots (my-last (take-while is-monotonic? a-inits))]
    (cond
      (empty? a-seq) '(())
      (= (count monots) (count a-seq)) (seq [(seq a-seq)])
      :else (cons monots (split-into-monotonics (drop (count monots) a-seq))))))

(defn permutations [a-set]
   (cond
     (empty? a-set) '(())
     (singleton? a-set) (list (seq a-set))
     :else
     (let [as-set (set a-set)
          perms-no-x (fn [x] (permutations (disj as-set x)))
          perms-x (fn [x] (map #(cons x %) (perms-no-x x)))]
            (apply concat (map #(perms-x %) as-set)))))

(defn add-into-sets [x sets]
  (map #(conj % x) sets))

(defn powerset [a-set]
  (let [x-set (set a-set)]
    (cond
      (empty? x-set) #{#{}}
      (singleton? x-set) #{x-set #{}}
      :else (let [powerset-removal (powerset (disj x-set (first x-set)))]
            (set (concat
                 (add-into-sets (first x-set) powerset-removal)
                  powerset-removal))))))
