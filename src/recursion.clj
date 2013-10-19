(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll))) (nil? (first (rest coll)))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn first-longer [seq-1 seq-2]
  (cond (empty? seq-1) false
        (empty? seq-2) true
        (first-longer (rest seq-1) (rest seq-2)) true
        :else false))

(defn seq-max [seq-1 seq-2]
  (if (first-longer seq-1 seq-2) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (not (pred? (first a-seq))) a-seq
        :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (or (empty? a-seq)
            (empty? b-seq))
          (and (empty? a-seq) (empty? b-seq))
        (not (= (first a-seq) (first b-seq)))
           false
        :else
          (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0) 1 (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

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

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a b]
  (if (empty? b)
    '()
    (cons (concat b a)
          (rotations-helper (concat a (seq [(first b)])) (rest b)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper '() a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a (first a-seq)
          afreq (if (nil? (freqs a))
                  0
                  (freqs a))]
      (my-frequencies-helper (assoc freqs a (inc afreq))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[elem freq] (first a-map)]
      (concat (repeat freq elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    [(my-take div a-seq) (my-drop div a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? b-seq) a-seq
        (empty? a-seq) b-seq
        :else (let [af (first a-seq)
                    bf (first b-seq)]
                (if (<= af bf)
                  (cons af (seq-merge (rest a-seq) b-seq))
                  (cons bf (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic?-helper [dir a-seq]
  (if (<= (count a-seq) 1)
    true
    (if (not (dir (first a-seq) (first (rest a-seq))))
      false
      (monotonic?-helper dir (rest a-seq)))))

(defn monotonic? [a-seq]
  (or (monotonic?-helper < a-seq)
      (monotonic?-helper > a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [div (last (take-while monotonic? (reverse (inits a-seq))))]
      (cons div (split-into-monotonics (drop (count div) a-seq))))))

(defn permutations-helper [tail left]
  (if (empty? left)
    (list tail)
    (apply concat (map (fn [elem]
                         (permutations-helper (cons elem tail)
                                              (disj left elem)))
                       left))))

(defn permutations [a-set]
  (permutations-helper '() (into #{} a-set)))

(defn powerset-helper [chosen left]
  (if (empty? left)
    (list chosen)
    (concat (powerset-helper chosen
                             (rest left))
            (powerset-helper (conj chosen (first left))
                             (rest left)))))

(defn powerset [a-set]
  (powerset-helper #{} a-set))

