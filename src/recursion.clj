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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (and (not (empty? a-seq))
           (pred? (first a-seq)))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    (list)))

(defn my-drop-while [pred? a-seq]
  (if (and (not (empty? a-seq)) (pred? (first a-seq)))
     (my-drop-while pred? (rest a-seq))
     a-seq))

(defn seq= [seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2))
      true
    (or (empty? seq-1) (empty? seq-2))
      false
    (= (first seq-1) (first seq-2))
      (seq= (rest seq-1) (rest seq-2))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (list)
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
  
(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
    (list)))

(defn my-range [up-to]
  (cond
    (= up-to 0)
      (list)
    (= up-to 1)
      (list 0)
    :else
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (seq a-seq)
    (cons (seq a-seq)
          (tails (rest a-seq)))
    (list a-seq)))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons (seq a-seq) (inits (drop-last a-seq)))))

(defn rotations
  ([a-seq pos]
    (cond
      (empty? a-seq)
        (list (list))
      (pos? pos)
        (cons (seq a-seq)
              (rotations (cons (last a-seq) (drop-last a-seq))
                         (dec pos)))))
  ([a-seq]
    (rotations a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [updated-count (inc (get freqs (first a-seq) 0))
          updated-frequencies (assoc freqs (first a-seq) updated-count)]
      (my-frequencies-helper updated-frequencies (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce-kv (fn [l k v] (concat (repeat v k) l)) (list) a-map))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    (list)
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (pos? n)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (cons (my-take half a-seq)
          (list (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (if (> (first a-seq) (first b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? (rest a-seq))
      a-seq
    (empty? (rest (rest a-seq)))
      (if (< (first a-seq) (second a-seq))
        (cons (first a-seq) (list (second a-seq)))
        (cons (second a-seq) (list (first a-seq))))
    :else
      (let [[fst snd] (halve a-seq)]
        (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn take-while-unary [pred a-seq]
  (cond
    (or (empty? a-seq) (singleton? a-seq))
      a-seq
    (pred (first a-seq) (second a-seq))
      (cons (first a-seq) (take-while-unary pred (rest a-seq)))
    :else
      (list (first a-seq))))

(defn split-into-monotonics [a-seq]
  (when (seq a-seq)
    (let [longest-monotonic (seq-max (take-while-unary < a-seq)
                                     (take-while-unary > a-seq))]
      (cons longest-monotonic (split-into-monotonics (drop (count longest-monotonic) a-seq))))))

(defn swap-elements [a-seq pos-1 pos-2]
  (let [v (vec a-seq)]
    (assoc v pos-1 (v pos-2) pos-2 (v pos-1))))

(defn permutations
  ([a-seq]
   (permutations a-seq 0))
  ([acc i]
   (if (= i (count acc))
     (list acc)
     (apply concat
            (for [j (range i (count acc))]
              (permutations (swap-elements acc i j)
                            (inc i)))))))

(defn powerset
  ([a-set]
   (conj (powerset a-set #{}) #{}))
  ([a-set init]
   (when (seq a-set)
     (concat
       (conj #{} (conj init (first a-set)))
       (powerset (rest a-set) (conj init (first a-set)))
       (powerset (rest a-set) init)))))




