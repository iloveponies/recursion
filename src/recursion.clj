(ns recursion
  (use [clojure.set :only [union]]))

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
    (singleton? coll) (first coll)
    (empty? coll) nil
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq) nil
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
          (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq) nil
    :else (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x (first a-seq)
          xs (rest a-seq)]
      (if (pred? x)
        (cons x (my-filter pred? xs))
        (my-filter pred? xs)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
      (nil? x) '()
      (not (pred? x)) '()
      :else (cons x (my-take-while pred? xs)))))

(defn my-drop-while [pred? a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (cond
      (nil? x) xs
      (pred? x) (my-drop-while pred? xs)
      :else (cons x xs))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq)
         (empty? b-seq)) true
    (not= (empty? a-seq)
          (empty? b-seq)) false
    :else (and (= (first a-seq)
                  (first b-seq))
               (seq= (rest a-seq)
                     (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1)
                   (first seq-2))
                (my-map f
                        (rest seq-1)
                        (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (rest (map concat
               (tails a-seq)
               (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (inc (get freqs k 0))
          freqs (assoc freqs k v)]
      (my-frequencies-helper freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [[k n] (first a-map)]
      (concat (repeat n k)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (and (zero? (count coll))
               (not (zero? n))))
    '()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (vector (my-take n a-seq)
            (my-drop n a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[xs ys] (halve a-seq)]
      (seq-merge (merge-sort xs)
                 (merge-sort ys)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [pred? (fn [ys] (or (apply < ys) (apply >= ys)))
          zs (drop 1 (reverse (inits a-seq)))  ; drop the empty list
          xs (last (take-while pred? zs))
          ys (drop (count xs) a-seq)]
      (cons xs (split-into-monotonics ys)))))

(defn permutations [xs]
  (if (empty? xs)
    '(())
    (mapcat
      (fn [x] (map (fn [y] (cons x y))
                   (permutations (remove (partial = x) xs)))) xs)))

(defn powerset [xs]
  (if (empty? xs)
    #{#{}}
    (let [[y ys] (map set (split-at 1 xs))
          zs (powerset ys)]
      (union zs (map (fn [x] (union x y)) zs)))))
