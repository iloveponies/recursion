(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (or (= coll [nil])
      (and (not (nil? (first coll)))
           (empty? (rest coll)))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [filtered-rest (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq))
        (cons (first a-seq) filtered-rest)
        filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-take-while pred? (rest a-seq)))
        :else ()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
      (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

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
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rot-iter [r-seq rs]
  (if (= rs 0)
    ()
    (cons r-seq (rot-iter (concat (rest r-seq)
                                  (cons (first r-seq) ()))
                          (dec rs)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rot-iter a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq)
                                  (inc (get freqs (first a-seq) 0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[x frequency] (first a-map)]
      (concat (repeat frequency x) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (or (empty? a-seq)
      (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) 
    '()
    (let [a-inits (inits a-seq)
          a-mono (last (take-while monotonic? a-inits))]
      (cons a-mono (split-into-monotonics (drop (count a-mono) a-seq))))))
          
(defn cons-all [x ys]
  (map (fn [y] (cons x y)) ys))

(defn permutations [a-set]
  (let [perm-rest (fn [x] (cons-all (first x) (permutations (rest x))))]
    (if (empty? a-set)
      '(())
      (apply concat (map perm-rest (rotations a-set))))))
        
(defn powerset [a-seq]
  (let [a-set (set a-seq)]
    (if (empty? a-set)
      #{#{}}
      (conj (apply clojure.set/union
                   (map (fn[x] (powerset (disj a-set x))) a-set)) a-set))) )

