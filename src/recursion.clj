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
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq) (empty? b-seq)
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (some empty? [seq-1 seq-2]) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations
  ([a-seq]
   (if (empty? a-seq)
     '(())
     (rotations (seq a-seq) (count a-seq))))
  ([a-seq n]
   (if (= n 0)
     '()
     (cons a-seq (rotations (concat (rest a-seq) [(first a-seq)]) (dec n))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (inc (or (get freqs (first a-seq)) 0))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [item (first a-map)]
    (if (nil? item)
      '()
      (concat (repeat (second item) (first item)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[seq-1 seq-2] (halve a-seq)]
      (seq-merge (merge-sort seq-1) (merge-sort seq-2)))))

(defn split-into-monotonics-helper [in-seq out-seq] ; in-seq is the sequence of remaining numbers. out-seq is the result sequence of monotonic sequences
  (if (empty? in-seq)
    out-seq
    (let [my-pred?  ; my-pred? returns true for x if it can be placed in the last sequence of out-seq
          (fn [x]
            (or (<= (count (last out-seq)) 1)                           ; if sequence is 0 or 1 of length, the direction isn't determined yet
                (<= (first (last out-seq)) (last (last out-seq)) x)     ; if first <= last item of sequnce, the sequence is increasing or the direction isn't determined yet
                (>= (first (last out-seq)) (last (last out-seq)) x)))]  ; if first >= last item of sequnce, the sequence is decreasing or the direction isn't determined yet
      (if (my-pred? (first in-seq)) ; if my-pred? returns true, place first item from in-seq into last sequence of out-seq, else place it in a new seq.
        (split-into-monotonics-helper (rest in-seq) (concat (butlast out-seq) [(concat (last out-seq) [(first in-seq)])]))
        (split-into-monotonics-helper (rest in-seq) (concat out-seq [(seq [(first in-seq)])]))))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper a-seq '()))

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) [a-set]
    :else (apply concat (map (fn [rot]
                               (map (fn [perm] (concat [(first rot)] perm))
                                    (permutations (rest rot))))
                             (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (clojure.set/union #{(set a-set)}
                       (apply clojure.set/union
                              (map (fn [rot] (powerset (rest rot)))
                                   (rotations a-set))))))


