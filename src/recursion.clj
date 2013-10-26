
(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (nil? (first coll)))
           (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (== 1 (count coll))
          (== 0 (count coll)))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [rec (fn f [a b]
              (cond
                (empty? a) "second"
                (empty? b) "first"
                :else (f (rest a) (rest b))))]

    (if (= "first" (rec seq-1 seq-2))
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (or (empty? (rest a-seq))
          (empty? a-seq))
    (first a-seq)
    (longest-sequence
      (cons
        (seq-max (first a-seq) (second a-seq))
        (rest (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (seq a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (and (empty? a-seq) (not (empty? b-seq)))
        (and (empty? b-seq) (not (empty? a-seq))))
      false
    :else (if (= (first a-seq) (first b-seq))
            (seq= (rest a-seq) (rest b-seq))
            false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
           (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== 1 n)  1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times
                 what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons (first [what-to-repeat])
          (my-repeat
            (dec how-many-times)
            what-to-repeat))))

(defn my-range [up-to]
  (if (== 0 up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (seq [()])
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [rotate
           (fn f [a-seq b-seq]
             (if (empty? a-seq)
               ()
               (cons (concat a-seq b-seq)
                     (f (rest a-seq)
                        (concat b-seq
                                (seq [(first a-seq)]))))))]
    (rotate a-seq (seq [])))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [frst (first a-seq)]
      (if (contains? freqs frst)
        (my-frequencies-helper (assoc freqs frst (inc (get freqs frst))) (rest a-seq))
        (my-frequencies-helper (assoc freqs frst 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [frst (first a-map)
          n (frst 1)
          thing (frst 0)]
      (concat (repeat n thing) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))
        vec [(my-take n a-seq)
             (my-drop n a-seq)]]
    vec))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    ()
    (let [a (first a-seq)
          b (first b-seq)]
      (cond
        (= nil a)  b-seq
        (= nil b)  a-seq
        (> a b)  (cons b (seq-merge a-seq (rest b-seq)))
        :else    (cons a (seq-merge (rest a-seq) b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (halves 0))
                 (merge-sort (halves 1))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

