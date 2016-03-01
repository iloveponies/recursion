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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [l1 (count seq-1)
        l2 (count seq-2)]
    (if (> l1 l2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
             (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [[f & r] a-seq]
    (cond
      (empty? a-seq)
      a-seq
      (pred? f)
        (cons f (my-take-while pred? r))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (let [[f & r] a-seq]
    (cond
      (empty? a-seq) a-seq
      (pred? f) (my-drop-while pred? r)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [[fa & ra] a-seq
        [fb & rb] b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (or (empty? a-seq) (empty? b-seq)) false
      (= fa fb) (seq= ra rb)
      :else false)))

(defn my-map [f seq-1 seq-2]
  (let [[f1 & r1] seq-1
        [f2 & r2] seq-2]
    (cond
      (or (empty? seq-1) (empty? seq-2)) '()
      :else (cons (f f1 f2) (my-map f r1 r2)))))

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
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [up-to-1 (dec up-to)]
    (cond
      (= up-to 0) '()
      :else (cons up-to-1 (my-range up-to-1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [h (first a-seq)]
      (my-frequencies-helper
        (if (get freqs h)
          (assoc freqs h (inc (get freqs h)))
          (assoc freqs h 1))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[k n] (first a-map)]
      (concat (repeat n k)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[fh sh] (halve a-seq)]
      (seq-merge (merge-sort fh) (merge-sort sh)))))

(defn monotonic? [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic (->> a-seq inits (take-while monotonic?) last)]
      (cons monotonic (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (mapcat (fn [e]
              (map #(cons e %)
                   (permutations (disj (set a-seq) e))))
            a-seq)))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [a-set (set a-set)
          e (first a-set)
          rest (powerset (disj a-set e))]
      (set (concat rest (map #(conj % e) rest))))))

