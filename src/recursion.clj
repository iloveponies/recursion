(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [first-of-seq (first a-seq)]
    (if (or (singleton? a-seq) (empty? a-seq))
      first-of-seq
      (max first-of-seq (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
    (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [first (first a-seq)
        rest (rest a-seq)]
  (cond 
    (empty? a-seq) a-seq
    (pred? first)
      (cons first (my-filter pred? rest))
    :else (my-filter pred? rest))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false                       
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [first (first a-seq)]
  (cond
   (empty? a-seq) ()                       
   (not (pred? first)) ()
   :else (cons first (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()                       
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or
      (empty? a-seq)
      (empty? b-seq)
      (not (= (first a-seq) (first b-seq)))) false 
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
   (if (empty? a-seq)
    freqs
    (let [first (first a-seq)
          freq (if (contains? freqs first)
                  (inc (get freqs first))
                  1)]
      (my-frequencies-helper (assoc freqs first freq)
                         (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [un-freqs a-map]
   (if (empty? a-map)
    un-freqs
    (let [first (first a-map)
          repeated (repeat (val first) (key first))]
      (un-frequencies-helper (concat un-freqs repeated) (rest a-map)))))

(defn un-frequencies [a-map]
   (un-frequencies-helper () a-map))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
    (cond 
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      :else (let [a-first (first a-seq)
                  b-first (first b-seq)]
              (if (< a-first b-first)
                (cons a-first (seq-merge (rest a-seq) b-seq))
                (cons b-first (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq)) a-seq
    (let [[first second] (halve a-seq)]
      (seq-merge (merge-sort first) (merge-sort second)))))

(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

