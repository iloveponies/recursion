(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (empty? coll) (empty? (rest coll)))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [rest-1 (rest seq-1)
        rest-2 (rest seq-2)]
    (cond
      (empty? rest-1) seq-2
      (empty? rest-2) seq-1
      ; exponential memory usage =(
      :else (first (seq-max (cons seq-1 (rest rest-1)) (cons seq-2 (rest rest-2)))))))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
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
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (cond
    (< up-to 1) '()
    (= up-to 1) '(0)
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [this-freq (get freqs (first a-seq))
          new-freq (if (number? this-freq)
                     (inc this-freq)
                     1)]
      (my-frequencies-helper (assoc freqs (first a-seq) new-freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map repeat (vals a-map) (keys a-map))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (/ (count a-seq) 2)]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [not-monotonic? (fn [nums] (not (or (apply <= nums) (apply >= nums))))
        inits (drop-last (inits a-seq))
        monotonic (first (drop-while not-monotonic? inits))
        non-monotonic (drop (count monotonic) a-seq)]
    (if (or (empty? (rest a-seq)) (empty? monotonic))
      nil
      (cons monotonic (split-into-monotonics non-monotonic)))))

(defn permutations [a-set]
  (if (empty? (rest a-set))
    [a-set]
    (let [sub-permutations (permutations (rest a-set))]
      (apply concat (map rotations (map cons (repeat (count sub-permutations) (first a-set)) sub-permutations))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [subset (powerset (rest a-set))
          element (first a-set)
          joint-set (fn [cur-set] (conj cur-set element))]
      (set (concat subset (map joint-set subset) #{#{element}})))))

