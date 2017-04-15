(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (not (== (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? n) 0
   (zero? k) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reversed (reverse (seq a-seq))]
    (if (empty? a-seq)
      '(())
      (cons (seq a-seq) (inits (reverse (rest reversed)))))))

(defn rotation-helper [a-seq n]
  (let [rotated (cons (first (reverse a-seq)) (reverse (rest (reverse a-seq))))]
    (if (<= n 0)
      (seq [rotated])
      (cons rotated (rotation-helper rotated (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (seq ['()])
    (rotation-helper a-seq (dec (count a-seq)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-item (first a-seq)
          new-val (if (contains? freqs first-item)
                    (inc (get freqs first-item))
                    1)
          new-freqs (assoc freqs first-item new-val)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
      '()
      (let [first-pair (first a-map)
            repeat-value (key first-pair)
            repeat-times (val first-pair)]
        (concat (repeat repeat-times repeat-value) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-point (int (/ (count a-seq) 2))]
    (cons (my-take half-point a-seq) [(my-drop half-point a-seq)])))

(defn seq-merge-helper [a-seq b-seq merged]
  (cond
   (empty? a-seq) (concat (reverse merged) b-seq)
   (empty? b-seq) (concat (reverse merged) a-seq)
    :else (let [a-item (first a-seq)
                b-item (first b-seq)]
            (if (< a-item b-item)
              (seq-merge-helper (rest a-seq) b-seq (cons a-item merged))
              (seq-merge-helper a-seq (rest b-seq) (cons b-item merged))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq '()))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (singleton? a-seq) (seq a-seq)
   :else (let [halves (halve a-seq)]
           (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (loop [a-seq a-seq
         current-mono '()
         monotonics '()]
    (if (empty? a-seq)
      (if (empty? current-mono)
        (reverse monotonics)
        (reverse (cons (reverse current-mono) monotonics)))
      (let [new-mono (cons (first a-seq) current-mono)]
        (if (or (singleton? new-mono) (monotonic? new-mono))
          (recur (rest a-seq) new-mono monotonics)
          (recur (rest a-seq) [(first a-seq)] (cons (reverse current-mono) monotonics)))))))

(defn permutations [a-set]
  (cond
   (singleton? a-set) [a-set]
   (empty? a-set) [[]]
   :else (for [item a-set
               rest-items (permutations (disj (set a-set) item))]
           (cons item rest-items))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [item (first a-set)
          powerset-rest (powerset (rest a-set))
          powerset-union (set (map (fn [x] (conj x item)) powerset-rest))]
      (clojure.set/union powerset-rest powerset-union))))
