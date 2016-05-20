(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (singleton? coll) (first coll)
    (empty? coll) nil
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (== (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (== 0 n) 0
    (== 0 k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (< n 2) n
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (tails a-seq) (reverse (rest (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-a-seq (first a-seq)
          new-freqs (assoc freqs first-a-seq (inc (get freqs first-a-seq 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [k (key (first a-map))
          v (val (first a-map))
          r (rest a-map)]
      (concat (repeat v k) (un-frequencies r)))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (== n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [len (count a-seq)
        half (int (/ len 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [[f, s] (halve a-seq)
        sf (if (or (empty? f) (singleton? f))
             f
             (merge-sort f))
        ss (if (or (empty? s) (singleton? s))
             s
             (merge-sort s))]
    (seq-merge sf ss)))

(defn monotonic? [sequence]
  (cond
    (< (count sequence) 2) false
    :else (or (apply <= sequence) (apply >= sequence))))

(defn split-into-monotonics [a-seq]
  (let [inits-seq (inits a-seq)
        monotonic (drop-while (fn [i] (not (monotonic? i))) inits-seq)]
    (cond
      (empty? monotonic) '()
      :else (let [last-m (first monotonic)
                  n (count last-m)]
              (cons last-m (split-into-monotonics (drop n a-seq)))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (cons (seq a-set) '())
    :else (for [head a-set
                tail (permutations (disj (set a-set) head))]
            (cons head tail))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (set (conj
           (for [item a-set
                tail (powerset (disj (set a-set) item))]
             (set tail ))
           (set a-set)))))

