(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
        (product (rest coll))))) 

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq] ; should be a better way to write this
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

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
    (= false (pred? (first a-seq)))
      ()
    :else
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (= true (pred? (first a-seq)))
      (my-drop-while  pred? (rest a-seq))
    :else
      (seq a-seq) ))

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
  (cond
    (or (empty? seq-1) (empty? seq-2))
      ()
    :else
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

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
    (cons a-seq ())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
  (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
    (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [a-seq (repeat (second (first a-map)) (first (first a-map)))]
      (concat a-seq (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    :else
    (let [final-seq (if (< (first a-seq) (first b-seq))
                            (first a-seq)
                            (first b-seq))]
    ;(cons final-seq (seq-merge a-seq (rest b-seq)))
      (if (= (last (vector final-seq)) (first b-seq))
        (cons final-seq (seq-merge a-seq (rest b-seq)))
        (cons final-seq (seq-merge (rest a-seq) b-seq)))
      )))

(defn merge-sort [a-seq]
  (if (or (= 0 (count a-seq)) (= 1 (count a-seq)))
    (sequence a-seq)
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn monotonic? [a-seq]
  (and (not (empty? a-seq)) (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (let [largest-prefix (last (filter monotonic? (inits a-seq)))
        prefix-length (count largest-prefix)]
    (if (empty? a-seq)
      ()
      (cons largest-prefix 
            (split-into-monotonics (drop prefix-length a-seq))))))

(defn flatten-levels [levels a-seq]
  (if (zero? levels)
    a-seq
    (flatten-levels (dec levels) (apply concat a-seq))))

(defn perm-helper [perm a-set]
  (if (empty? a-set)
    perm
    (map (fn [a] (perm-helper (conj perm a) (remove #{a} a-set))) a-set)))

(defn permutations [a-set]
  (if (empty? a-set)
    '([])
(flatten-levels (dec (count a-set)) (perm-helper [] a-set))))


(permutations #{10})

(defn add-to-all [a-set elem]
  (if (empty? a-set)
    a-set
    (let [new-set (conj (first a-set) elem)]
      (set (cons new-set (add-to-all (rest a-set) elem))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (set ( cons #{} a-set))
    (set (concat (powerset (rest a-set)) 
         (add-to-all (powerset (rest a-set)) (first a-set))))))

