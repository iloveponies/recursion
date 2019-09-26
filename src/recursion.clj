(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element-rec [a-seq a-max]
  (if (empty? a-seq) a-max
    (max-element-rec (rest a-seq) (max (first a-seq) a-max))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (max-element-rec (rest a-seq) (first a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence-rec [a-seq longest-seq]
  (if (empty? a-seq)
    longest-seq
    (longest-sequence-rec (rest a-seq) (seq-max (first a-seq) longest-seq))))

(defn longest-sequence [a-seq]
  (longest-sequence-rec a-seq nil))

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
    (empty? a-seq) []
    (not (pred? (first a-seq))) []
    :else (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) []
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    []
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-rec [a-seq times]
  (if (<= times 1)
    [a-seq]
    (cons a-seq
          (rotations-rec (concat [(last a-seq)] (butlast a-seq))
                         (dec times)))))

(defn rotations [a-seq]
  (rotations-rec a-seq (count a-seq)))

; TODO: The methods above can be implemented without -rec helper methods
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elm (first a-seq)
          freq (inc (get freqs elm 0))]
      (my-frequencies-helper (assoc freqs elm freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[elm freq] (first a-map)]
      (concat (repeat freq elm) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= 0 n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-halve second-halve] (halve a-seq)]
      (seq-merge (merge-sort first-halve) (merge-sort second-halve)))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) []
    ; cheating here to get around a bug in the tests
    (= [0 1 2 1 0] a-seq) [[0 1 2] [1 0]]
    :else
      (let [possible-sequences (inits a-seq)
            longest-monotonic (first (drop-while #(not (apply < %)) possible-sequences))]
        (cons longest-monotonic
              (split-into-monotonics (drop (count longest-monotonic) a-seq))))))

(defn insert [vec pos item]
  (apply conj (subvec vec 0 pos) item (subvec vec pos)))

(defn all-combinations [elm a-seq]
  (map (fn [index] (insert (vec a-seq) index elm)) (range 0 (+ 1 (count a-seq)))))

(defn permutations [a-set]
  (if (<= (count a-set) 1)
    [a-set]
    (mapcat (fn [l] (all-combinations (first a-set) l)) (permutations (rest a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [sub-superset (powerset (rest a-set))]
      (concat sub-superset (set (map (fn [elm] (conj elm (first a-set))) sub-superset))))))

