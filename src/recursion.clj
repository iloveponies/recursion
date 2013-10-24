(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq)
        (if(nil? (max-element (rest a-seq))) ;nil can't be passed to max
          (Integer/MIN_VALUE)                ;so if we get nil from max-element
          (max-element (rest a-seq))))))     ;then use Integer/MIN_VALUE

(defn seq-max [seq-1 seq-2]
  (let [seq-1-lenght (count seq-1)
        seq-2-lenght (count seq-2)]
    (if (> seq-1-lenght seq-2-lenght)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? fst)
        (cons fst (my-filter pred? rst))
        (my-filter pred? rst)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (empty? a-seq) false
   (empty? b-seq) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

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
  (if ((complement pos?) how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (if (empty? a-seq)
      (cons '() '())
      (cons (seq a-seq) (tails rst)))))

(defn inits [a-seq]
  (reverse (map reverse(tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [tai (tails a-seq)
        ini (inits a-seq)
        lenght (count a-seq)]
    (if (empty? a-seq)
      '(())
      (take lenght (map concat tai ini)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (inc (freqs (first a-seq)))) (rest a-seq))
   :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [lenght (count a-seq)
        take-lenght (int (/ lenght 2))]
    [(my-take take-lenght a-seq) (my-drop take-lenght a-seq)]))

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


















