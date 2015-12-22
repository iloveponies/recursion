(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a0 (first a-seq), a-rest (rest a-seq)]
      (if (pred? a0)
        (cons a0 (my-filter pred? a-rest))
        (my-filter pred? a-rest)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
   false
   (= elem (first a-seq))
   true
   :else
   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
   true
   (or (empty? a-seq) (empty? b-seq) (not (= (first a-seq) (first b-seq))))   
   false
   :else
   (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
   ()
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
   n
   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (reverse (rest (reverse (my-map concat (tails a-seq) (inits a-seq)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key       (first a-seq)
          val       (if (contains? freqs key) (inc (get freqs key)) 1)
          new-freqs (assoc freqs key val)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[k v] (first a-map)]
      (if (< v 1)
        (un-frequencies (dissoc a-map k))
        (cons k (un-frequencies (assoc a-map k (dec v))))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-len (int (/ (count a-seq) 2))]
    [(my-take half-len a-seq) (my-drop half-len a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else
   (let [a0 (first a-seq)
         b0 (first b-seq)]
     (if (< a0 b0)
       (cons a0 (seq-merge (rest a-seq) b-seq))
       (cons b0 (seq-merge a-seq        (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [init-seq (rest (inits a-seq))
          up-seq  (last (take-while (fn [s] (apply <= s)) init-seq))
          dn-seq  (last (take-while (fn [s] (apply >= s)) init-seq))
          up-len  (count up-seq)
          dn-len  (count dn-seq)
          [mon-seq mon-len] (if (> up-len dn-len) [up-seq up-len] [dn-seq dn-len])]
      (cons mon-seq (split-into-monotonics (drop mon-len a-seq))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    [a-set]
    (let [rots              (rotations a-set)
          first-fixed-perms (fn [s] (map (fn [ss] (cons (first s) ss)) (permutations (rest s))))]
      (apply concat (map first-fixed-perms rots )))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (set (apply concat (map (fn [s] [(set (cons (first a-set) s)) s]) (powerset (rest a-set)))))))

