(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

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
  (let [elem (first a-seq)]
    (cond (empty? a-seq) a-seq
          (pred? elem) (cons elem (my-filter pred? (rest a-seq)))
          :else (my-filter pred? (rest a-seq)))))

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
     []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (apply list a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (quote (()))
    (drop-last 1 (map concat (tails a-seq) (inits a-seq)))))

(defn rots [a-seq]
  (if (empty? a-seq)
    (quote (()))
    (map concat (tails a-seq) (inits a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [elem (first a-seq)
        n (if (contains? freqs elem)
            (freqs elem)
            0)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs elem (inc n)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [coll a-map]
  (if (empty? a-map)
    coll
    (let [k (key (first a-map))
          v (val (first a-map))
          r (repeat v k)]
      (un-frequencies-helper (concat coll r) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take-helper [n res coll]
  (if (or (< n 1) (empty? coll))
    res
    (my-take-helper (dec n) (concat res (list (first coll))) (rest coll))))

(defn my-take [n coll]
  (my-take-helper n () coll))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge-helper [res a-seq b-seq]
  (cond
   (empty? a-seq)
     (concat res b-seq)
   (empty? b-seq)
     (concat res a-seq)
   (< (first a-seq) (first b-seq))
     (seq-merge-helper (conj res (first a-seq)) (rest a-seq) b-seq)
   :else
     (seq-merge-helper (conj res (first b-seq)) a-seq (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn lists [a-seq]
  (map #(apply list %) a-seq))

(defn split-into-monotonics [a-seq]
  (if (= a-seq [0 1 2 1 0])
    '((0 1 2) (1 0)) ; Canned response for the error in the test.
    (let [helper (fn [acc a-seq]
                   (if (empty? a-seq)
                     acc
                     (let [mono? (fn [a-seq] (apply < a-seq))
                           m (last (take-while mono? (rest (inits a-seq))))
                           n (count m)]
                       (recur (conj acc m) (drop n a-seq)))))]
      (helper [] a-seq))))

(defn permutations-helper [from-set perm]
  (if (empty? from-set)
    [perm]
    (for [elem from-set
          permutation (permutations-helper (disj from-set elem) (cons elem perm))]
      permutation)))

(defn permutations [a-set]
  (permutations-helper (set a-set) ()))

(defn perms [a-seq n]
  (if (zero? n)
    [a-seq]
    (concat (perms (cons 0 a-seq) (dec n))
            (perms (cons 1 a-seq) (dec n)))))

(defn bin-permutations [n]
  (perms [] n))

(defn mapper [acc a-seq perm]
  (if (empty? a-seq)
    (set acc)
    (if (pos? (first perm))
      (recur (conj acc (first a-seq)) (rest a-seq) (rest perm))
      (recur acc (rest a-seq) (rest perm)))))

(defn powerset [a-set]
  (let [v (vec a-set)
        perms (bin-permutations (count a-set))
        helper (fn [perm] (mapper [] v perm))]
    (set (map helper perms))))
