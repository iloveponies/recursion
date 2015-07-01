(ns recursion)


(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (= () (rest coll))
      true
      false)))


(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (let [eka (first a-seq)
            vika (my-last a-seq)]
        (max eka vika)
        (max-element (rest a-seq))))))


(defn seq-max [seq-1 seq-2]
  (let [seq-1-len (count seq-1)
        seq-2-len (count seq-2)]
    (if (> seq-1-len seq-2-len)
      seq-1
      (if (< seq-1-len seq-2-len)
        seq-2
        (if (== seq-1-len seq-2-len)
          seq-2)))))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (let [eka (first a-seq)
            vika (my-last a-seq)]
        (seq-max eka vika)
        (longest-sequence (rest a-seq))))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (= (first (map pred? a-seq)) true)
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


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (= (first (map pred? a-seq)) true)
      (cons (first a-seq)
      (my-take-while pred? (rest a-seq)))
      ())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (= (first (map pred? a-seq)) false)
      (cons (first a-seq) (rest a-seq))
      (my-drop-while pred? (rest a-seq)))))


(defn seq= [a-seq b-seq]
  (cond
   (= a-seq b-seq) true
   :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (if (and (not (= (first (map f seq-1)) nil)) (not (= (first (map f seq-2)) nil)))
      (cons (f (first (map f seq-1)) (first (map f seq-2)))
      (my-map f (rest seq-1) (rest seq-2))))))


(defn power [n k]
  (if (zero? n)
    0
    (if (zero? k)
      1
      (* n (power n (dec k))))))


(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (or (neg? how-many-times) (zero? how-many-times))
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (conj a-seq ())
    (cons (conj (rest a-seq) (first a-seq))
          (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    (conj a-seq ())
    (cons (reverse a-seq)
          (inits (rest a-seq)))))


(defn rotations [a-seq]
  [:-])


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-count (if (not (contains? freqs (first a-seq)))
                      (assoc freqs (first a-seq) 1)
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))))]
      (my-frequencies-helper new-count (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  [:-])


(defn my-take [n coll]
  (if (zero? n)
    ()
    (if (= (first coll) nil)
      ()
      (cons (first coll)
            (my-take (dec n) (rest coll))))))


(defn my-drop [n coll]
  (if (or (zero? n) (> n (count coll)))
    ()
    (cons (get coll n)
          (my-drop (inc n) (rest coll)))))


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

