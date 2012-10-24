(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond 
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
   (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) 
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (and (not (empty? a-seq)) 
       (or (== elem (first a-seq)) 
           (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (or (and (empty? a-seq) 
           (empty? b-seq)) 
      (and (not (empty? a-seq))
           (not (empty? b-seq))
           (== (first a-seq) (first b-seq)) 
           (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1 
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ 
     (fib (- n 2)) 
     (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
   '()
   (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range2 [soF up-to]
  (if (< soF 0)
  '()
  (cons soF (my-range2 (dec soF) up-to))))

(defn my-range [up-to]
 (if (>= 0 up-to)
  '()
  (my-range2 (- up-to 1) up-to)))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-help [rot-so-far inits tail ln]
  (if (empty? inits)
    rot-so-far
    (let [init-considered (first inits)
          length-of-tail (- ln (count init-considered))
          filtering-f (fn [t] (== (count t) length-of-tail))
          the-tail (first (filter filtering-f tail))
          new-rot (concat the-tail init-considered)
          new-list (cons new-rot rot-so-far)]
      (rotations-help new-list (rest inits) tail ln))))

(defn rotations [a-seq]
  (let [ln (count a-seq)]
    (if (empty? a-seq)
      ['()]
      (let [filter-empty (fn [t] (not (empty? t)))
            non-empty-inits (filter filter-empty (inits a-seq))]
       (rotations-help [] non-empty-inits (tails a-seq) ln)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [next-element (first a-seq)
          amount-of-times-seen (if (contains? freqs next-element) 
                                 (get freqs next-element) 
                                 0)
          new-freqs (assoc freqs next-element (+ 1 amount-of-times-seen))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-h [a-map a-seq]
  (if (empty? a-map)
    a-seq
    (let [first-entry (first a-map)
          next-el (first first-entry)
          freq (last first-entry)
          rest-of-map (rest a-map)
          new-list (repeat freq next-el)
          new-seq (concat new-list a-seq)]
      (un-frequencies-h rest-of-map new-seq))))

(defn un-frequencies [a-map]
  (un-frequencies-h a-map []))

(defn my-take [n coll]
  (if (or (empty? coll) (== 0 n))
    []
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (== 0 n))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [len (int (/ (count a-seq) 2))
        first-half (my-take len a-seq)
        second-half (my-drop len a-seq)]
    [first-half second-half]))

(defn merg-h [final-seq a-seq b-seq]
  (if (and (not (empty? a-seq)) (not (empty? b-seq)))
    (let [new-el (min (first a-seq) (first b-seq))
          new-a-seq (if (<= (first a-seq) (first b-seq)) (rest a-seq) a-seq)
          new-b-seq (if (> (first a-seq) (first b-seq)) (rest b-seq) b-seq)
          new-list (conj final-seq new-el)]
      (merg-h new-list new-a-seq new-b-seq))
    (if (empty? a-seq) 
      (concat final-seq b-seq)
      (concat final-seq a-seq))))

(defn seq-merge [a-seq b-seq]
  (merg-h [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])