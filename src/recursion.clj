(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (and ((complement empty?) coll)
       (empty? (rest coll))))


(defn my-last [coll]
  (cond
      (empty? coll) nil
      (singleton? coll) (first coll)
      :else (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? a-seq)
      nil
      (apply max a-seq)))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))


(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max a-seq (longest-sequence (rest a-seq)))
    ))


(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    ((complement pred?) (first a-seq)) ()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    ((complement pred?) (first a-seq)) a-seq
    :else (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    ((complement =) (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
    (= k 0) 1
    (= n 0) 0
    :else (* n (power n (dec k)))))


(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    :else (cons what-to-repeat
                (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (<= up-to 0)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
      (cons a-seq ())
      (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
      (cons a-seq ())
      (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))


(defn rotations [a-seq]
  (let [ts (vec (tails a-seq))
        is (vec (reverse (inits a-seq)))]
    (seq (set (map concat ts is)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (let [el (first a-seq)]
        (if (contains? freqs el)
          (assoc freqs el (inc (freqs el)))
          (assoc freqs el 1)))
      (rest a-seq))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (let [pair (first a-map)]
    (if (empty? pair)
      {}
      (concat (repeat (second pair) (first pair))
              (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
      coll
      (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq) (my-drop mid a-seq)]))


(defn seq-merge [a-seq b-seq]
  (cond
    (or (empty? a-seq) (empty? b-seq)) (concat a-seq b-seq)
    (<= (first a-seq) (first b-seq)) (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq))
    (< (first b-seq) (first a-seq)) (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))
    ))


(defn merge-sort [a-seq]
  (let [halves (halve a-seq)]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))




(defn monotonic? [a-seq]
  (if (empty? a-seq)
      true
      (or (apply > (vec a-seq)) (apply < (vec a-seq)))))


(defn monotonic-helper [a-seq monos]
  (let [ combs (reverse (inits a-seq))
         mn (last (take-while monotonic? combs))]
    (if (empty? a-seq)
      monos
      (monotonic-helper (drop (count mn) a-seq) (cons mn monos)))))


(defn split-into-monotonics [a-seq]
  (reverse (monotonic-helper a-seq '())))




(defn add-to-every [el a-seq]
  (map (fn [x] (concat [el] x)) a-seq))


(defn permutations [a-set]
  (cond
    (<= (count a-set) 2) (rotations a-set)
    :else (apply concat
           (map (fn [x] (add-to-every (first x) (permutations (set (rest x)))))
             (rotations a-set)))))




(defn convert-to-binary-list [x]
  (cond
    (= x 0) '(0)
    (= x 1) '(1)
    :else (if (= 1 (mod x 2))
            (cons 1 (convert-to-binary-list (int (/ x 2))))
            (cons 0 (convert-to-binary-list (/ x 2))))))

(defn bit-list [x]
  (reverse (convert-to-binary-list x)))

(defn prepend-zeros [x desired-length]
  (let [missing (- desired-length (count x))]
     (if (> missing 0)
       (concat (repeat missing 0) x)
       x)))

(defn filter-indices [switches a-seq]
  (filter
    (complement nil?)
    (map (fn [x y] (if (= x 1) y)) switches a-seq)))

(defn powerset [a-set]
  (let [cnt (power 2 (count a-set))
        rng (range 0 cnt)
        blist (map bit-list rng)
        plist (map (fn [x] (prepend-zeros x (count a-set))) blist)]
      (set (map set
                (map (fn [x] (filter-indices x a-set)) plist)))))
