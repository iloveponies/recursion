(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? (rest coll)) (first (rest coll))
   :else (my-last (rest coll))))

(defn max-recur [mx coll]
  (if (empty? coll)
    mx
    (max-recur (max mx (first coll)) (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
      (max-recur (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1))
    seq-2
    seq-1))

(defn longest-sequence-recur [curr-longest a-seq]
  (if (empty? a-seq)
    curr-longest
    (let [candidate (first a-seq)
          new-curr (if (<= (count candidate) (count curr-longest)) curr-longest candidate)]
      (longest-sequence-recur new-curr (rest a-seq)))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
      (longest-sequence-recur (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
     (= elem (first a-seq))
     true
     :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while-recur [prefix pred? a-seq]
  (if
   (or (empty? a-seq) (not (pred? (first a-seq)))) prefix
   (my-take-while-recur (conj prefix (first a-seq)) pred? (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (my-take-while-recur [] pred? a-seq))

(defn my-drop-while [pred? a-seq]
  (if
      (or (empty? a-seq) (not (pred? (first a-seq)))) a-seq
   (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
 (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map-recur
  [result f seq-1 seq-2]
  (if
      (or (empty? seq-1) (empty? seq-2)) result
      (my-map-recur
       (conj result (f (first seq-1) (first seq-2)))
       f
       (rest seq-1)
       (rest seq-2))))

(defn my-map [f seq-1 seq-2]
  (my-map-recur [] f seq-1 seq-2))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else
   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat-recur [result how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (my-repeat-recur (conj result what-to-repeat) (- how-many-times 1) what-to-repeat)
    result))

(defn my-repeat [how-many-times what-to-repeat]
  (my-repeat-recur [] how-many-times what-to-repeat))

(defn my-range-recur [result up-to]
  (if (> up-to 0)
    (my-range-recur (conj result (- up-to 1)) (- up-to 1))
    result))

(defn my-range [up-to]
  (my-range-recur [] up-to))

(defn tails-recur [result a-seq]
  (if (empty? a-seq)
    (conj result a-seq)
    (tails-recur (conj result a-seq) (rest a-seq))))

(defn tails [a-seq]
  (tails-recur [] a-seq))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotate [n a-seq]
  (let [c (count a-seq)]
    (take c (drop (mod n c) (cycle a-seq)))))

(defn rotations [a-seq]
  (if (>= 1 (count  a-seq)) (list a-seq)
      (map (fn [n] (rotate n a-seq)) (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [update-freqs (fn [elem]
                       (let [ct (get freqs elem 0)]
                         (assoc freqs elem (+ ct 1))))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (update-freqs (first a-seq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (if (empty? a-map)
    result
    (let [[elem ct] (first a-map)
          updated-result (concat result (my-repeat ct elem))]
      (un-frequencies-helper updated-result (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper {} a-map) )

(defn my-take-helper [res n coll]
  (if (or (>= 0 n) (empty? coll))
    res
    (my-take-helper (conj res (first coll)) (- n 1) (rest coll))))

(defn my-take [n coll]
  (my-take-helper [] n coll))

(defn my-drop [n coll]
  (if (or (>= 0 n) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [pivot (int (/ (count a-seq) 2))]
    [(my-take pivot a-seq) (my-drop pivot a-seq)]))

(defn seq-merge-helper [res a-seq b-seq]
  (cond
   (empty? a-seq) (concat res b-seq)
   (empty? b-seq) (concat res a-seq)
   :else
   (if (< (first a-seq) (first b-seq))
     (seq-merge-helper (conj res (first a-seq)) (rest a-seq) b-seq)
     (seq-merge-helper (conj res (first b-seq)) a-seq (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [[seq1 seq2] (halve a-seq)]
      (seq-merge (merge-sort seq1) (merge-sort seq2)))))

(defn split-into-monotonics-helper [res a-seq]
  (let [monotonic? (fn  [a-seq]
                     (or (apply <= a-seq)
                         (apply >= a-seq)))]
    (if (empty? a-seq)
      res
      (let [next-seq (last (take-while monotonic? (drop 1 (inits a-seq))))
            next-a-seq (drop (count next-seq) a-seq)]
        (split-into-monotonics-helper (conj res next-seq) next-a-seq)))))
  
(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper [] a-seq))

(defn remove-at [n xs]
  (let [[front after] (split-at (inc  n) xs)
        before (first (split-at (dec (count front)) front))]
     (concat before after)))

(defn permutations-n [n xs]
  (cond
   (= 0 n) (list (list))
   (= 1 n) (map list xs)
   :else (reduce (fn [collector i]
                   (let [elem (list (first (last (split-at i xs))))
                         sans-elem (remove-at i xs)
                         sub-perms (permutations-n (count sans-elem) sans-elem)]
                     (concat collector (map (fn [sub-perm] (concat elem sub-perm)) sub-perms))))
                 '() (range 0 (count xs)))
    ))

(defn permutations [xs]
  (permutations-n (count xs) xs))

(defn powerset [a-seq]
  (let [a-set (set a-seq)]
    (if (empty? a-set)
      #{#{}}
      (let [elements (map identity a-set)
            subsets-with-one-missing-element (map (fn [e] (disj a-set e)) elements)
            next-level (reduce (fn [collector sub]
                                 (let [sub-powerset (powerset sub)]
                                   (concat collector sub-powerset)))
                               #{}
                               subsets-with-one-missing-element)]
        (set (conj next-level a-set)))
      )))

