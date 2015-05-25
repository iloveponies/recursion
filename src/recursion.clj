(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

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
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq))   (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if
    (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n
           ]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
 (if
   (empty? a-seq)
   (cons () a-seq)
   (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [sq n]
  (if (= n 0)
    ()
    (cons (seq sq) (rotate (conj (drop-last sq) (last sq)) (- n 1)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ()) ; need to find better way
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          num (get freqs fst)
          update-freqs (fn [v]
                         (if num
                          (assoc freqs v (+ 1 num))
                           (assoc freqs v 1)))]
      (my-frequencies-helper (update-freqs fst) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn uf-helper [mp acc]
  (if (empty? mp)
    acc
    (let [[v n] (first mp)]
      (uf-helper (rest mp) (concat acc (my-repeat n v))))))

(defn un-frequencies [a-map]
  (uf-helper a-map {}))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [midpoint (quot (count a-seq) 2)]
    [(my-take midpoint a-seq) (seq (my-drop midpoint a-seq))]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[l-seq r-seq] (halve a-seq)]
      (seq-merge (merge-sort l-seq) (merge-sort r-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

