(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [me (max-element (rest a-seq))]
      (if (nil? me)
        (first a-seq)
        (max (first a-seq) me)))))



(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [max-el
        (fn ml [seq acc]
          (if (empty? seq)
            acc
            (ml (rest seq) (seq-max acc (first seq)))))]
    (max-el (rest a-seq) (first a-seq))))

(defn my-filter [pred? a-seq]
  (let [helper
        (fn fltr [pred? a-seq acc]
          (if (empty? a-seq)
            acc
            (if (pred? (first a-seq))
              (fltr pred? (rest a-seq) (conj acc (first a-seq)))
              (fltr pred? (rest a-seq) acc))))]
    (helper pred? a-seq [])))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    ()))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))


(defn rotations [a-seq]
  (let [helper
        (fn rt [a-seq acc]
          (if (empty? a-seq)
            ()
            (cons (concat a-seq acc) (rt (rest a-seq) (conj acc (first a-seq))))))]
    (helper a-seq [])))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (merge freqs {(first a-seq) (if (contains? freqs (first a-seq)) (+ 1 (get freqs (first a-seq))) 1)}) (rest  a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [fst (first a-map)
          k (first fst)
          v (second fst)]
    (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

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

