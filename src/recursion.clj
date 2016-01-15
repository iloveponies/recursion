(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
      :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
      seq-2
      seq-1
    ))

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
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
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
  (if (zero? n)
    0
    (if (zero? k)
      1
      (if (= 1 k)
        n
        (* n (power n (- k 1)))))))

(defn fib [n]
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
    (if(< how-many-times 1)
      ()
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< 0 up-to)
    (cons (dec up-to) (my-range (dec up-to)))
    ()))

(defn tails [a-seq]
  (if (= () a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
      (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (= freqs nil)
                      {(first a-seq) 1}
                      (assoc freqs (first a-seq) (inc(get freqs (first a-seq) 0)))
                      )]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (un-frequencies (rest a-map)) (repeat (second (first a-map)) (first (first a-map))))))

(defn my-take [n coll]
  (if (empty? coll)
    ()
    (if (<= n 0)
      ()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if (<= n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (if (> (first a-seq) (first b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) ()
   (empty? (rest a-seq)) a-seq
   :else (apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

