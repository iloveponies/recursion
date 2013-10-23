(ns recursion
  (:require [clojure.set :as set]))

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll)        nil
        (empty? (rest coll)) (first coll)
        :else                (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
      nil
      (let [[a & a-seq] a-seq]
        (if (empty? a-seq)
            a
            (let [[b & a-seq] a-seq]
              (max-element (cons (max a b) a-seq)))))))

(defn seq-max [seq-1 seq-2]
  ((fn [t-1 t-2]
     (cond (empty? t-1) seq-2
           (empty? t-2) seq-1
           :else        (recur (rest t-1) (rest t-2))))
   seq-1 seq-2))

(defn longest-sequence [seqs]
  ((fn [tails]
     (let [remains (filter (fn [[seq tail]] (not (empty? tail)))
                           (map vector seqs tails))]
       (cond (empty? remains) nil
             (empty? (rest remains)) (get (first remains) 0)
             :else (recur (map rest tails)))))
   seqs))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [[a & tail] a-seq]
      (if (pred? a)
        (cons a (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (and (not (empty? a-seq))
       (or (= elem (first a-seq))
           (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
      ()
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
      a-seq
      (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
      (and (empty? a-seq) (empty? b-seq))
      (and (= (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      ()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1 (* n (power n (dec k)))))

(defn fib [n]
  (cond (<= n 0) 0
        (=  n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  ((fn [acc n] (if (< n 0) acc (recur (conj acc n) (dec n))))
   [] (dec up-to)))

(defn tails [a-seq]
  (if (empty? a-seq) [[]] (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [a-seq (vec a-seq)]
      (reduce (fn [acc i]
                (conj acc (concat (subvec a-seq i (count a-seq))
                                  (subvec a-seq 0 i))))
              () (range 0 (count a-seq))))))

(defn my-frequencies [a-seq]
  (reduce (fn [freqs a] (assoc freqs a (inc (get freqs a 0))))
          {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat (map repeat (map second a-map) (map first a-map))))

(defn my-take [n coll]
  ((fn [acc n coll]
     (if (or (<= n 0) (empty? coll))
       acc 
       (recur (conj acc (first coll)) 
              (dec n) (rest coll))))
   [] n coll))

(defn my-drop [n coll]
  (if (<= n 0) coll (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq)
     (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  ((fn [acc a-seq b-seq]
     (cond (empty? a-seq)
             (concat acc b-seq)
           (empty? b-seq)
             (concat acc a-seq)
           (<= (first a-seq) (first b-seq))
             (recur (conj acc (first a-seq)) (rest a-seq) b-seq)
           :else
             (recur (conj acc (first b-seq)) a-seq (rest b-seq))))
   [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[as bs] (halve a-seq)]
      (seq-merge (merge-sort as)
                 (merge-sort bs)))))

(defn split-into-monotonics [a-seq]
  (if-let [mono (last (take-while (fn [init] (some #(apply % init) [<= >=]))
                                  (rest (inits a-seq))))]
    (cons mono (split-into-monotonics (drop (count mono) a-seq)))))

(defn permutations [a-set]
  (let [a-vec (vec a-set)]
    (if (empty? a-vec)
      [[]]
      (reduce (fn [acc i]
                (concat acc
                  (let [pivot (get a-vec i)
                        others (concat (subvec a-vec 0 i) (subvec a-vec (inc i)))]
                    (map (fn [rotation] (cons pivot rotation))
                         (permutations others)))))
              () (range 0 (count a-vec))))))

(defn powerset [a-set]
  (let [a-set (set a-set)]
    (reduce (fn [acc a]
              (let [others (powerset (disj a-set a))]
                (set/union acc (set/union others (map #(conj % a) others)))))
            #{#{}} a-set)))
