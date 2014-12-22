(ns recursion)

(defn product [coll]
  (if (empty? coll) 
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))


(defn my-last [coll]
  (if (empty? coll) 
    nil
      (if (singleton? coll)
        (first coll)
        (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq) nil (apply max a-seq)))

(defn longest-sequence [a-seq]
  (first (reverse (sort-by count a-seq))))

(defn seq-max [seq-1 seq-2]
  (longest-sequence [seq-1 seq-2]))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [h (first a-seq)
          t (rest a-seq)]
          (if (pred? h)
            (cons h (my-filter pred? t))
            (my-filter pred? t)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not (= (first a-seq) elem))
        (sequence-contains? elem (rest a-seq))
    :else
        true)) 

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq))
        (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else 
        []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false    ;;if only one is empty that means they're not equal
    (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) 
        []
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
    (cond
        (zero? n) 0
        (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
    (if (empty? a-seq)
        '(())
        (map (fn [n] (concat (drop n a-seq) (take n a-seq))) (range 0 (count a-seq))))) 

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)
          c (or (get freqs f) 0)
          new-freq (assoc freqs f (inc c))]
          (my-frequencies-helper new-freq (rest a-seq)))))
    

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map #(repeat (get a-map %) %) (keys a-map))))

(defn my-take [n coll]
  (cond
    (empty? coll) []
    (zero? n) []
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

