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
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

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
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (if
    (or (empty? a-seq) (not (pred? (first a-seq))))
      a-seq
      (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      ()
    :else
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

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
    (if (empty? a-seq)
      (cons (reverse a-seq) ())
      (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (distinct (map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-key (first a-seq)
          a-val (if (contains? freqs a-key) (get freqs a-key) 0)
          new-freqs (assoc freqs a-key (inc a-val))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [first-pair (first a-map)
          new-seq (concat a-seq (concat (repeat (val first-pair) (key first-pair))))]
      (un-frequencies-helper new-seq (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map))

(defn my-take-helper [n coll a-seq]
  (if (or (empty? coll) (< n 1))
    a-seq
    (let [new-coll (cons (first coll) a-seq)]
      (my-take-helper (dec n) (rest coll) new-coll))))

(defn my-take [n coll]
  (reverse (my-take-helper n coll ())))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [coll]
  (let [n (int (/ (count coll) 2))]
    (vector (take n coll) (drop n coll))))

(defn seq-merge-helper [a-seq b-seq ret-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (cond
      (empty? a-seq)
        (concat (reverse ret-seq) b-seq)
      (empty? b-seq)
        (concat (reverse ret-seq) a-seq)
      :else
        ret-seq)
    (if (< (first a-seq) (first b-seq))
      (let [new-seq (cons (first a-seq) ret-seq)]
        (seq-merge-helper (rest a-seq) b-seq new-seq))
      (let [new-seq (cons (first b-seq) ret-seq)]
        (seq-merge-helper a-seq (rest b-seq) new-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq ()))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (<= (count a-seq) 1))
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (last halves))))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics-helper [a-seq monotonics]
  (if (empty? a-seq)
    monotonics
    (let
      [seq-inits (rest (reverse (inits a-seq)))
       last-monotonic (last (filter monotonic? seq-inits))
       new-monotonics (cons last-monotonic monotonics)
      ]
      (split-into-monotonics-helper (drop (count last-monotonic) a-seq) new-monotonics))))

(defn split-into-monotonics [a-seq]
  (reverse (split-into-monotonics-helper a-seq ())))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (apply concat (map (fn [x] (map cons (repeat (first x)) (permutations (rest x)))) (rotations a-set)))))

(defn powerset [a-set]
  [:-])

