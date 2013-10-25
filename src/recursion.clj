(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

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
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [len-a (count seq-1)
        len-b (count seq-2)]
    (if (< len-a len-b)
      seq-2
      (if (> len-a len-b)
        seq-1
        seq-2))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

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
    '()
    (let [first-elem (first a-seq)]
      (if (pred? first-elem)
        (cons first-elem (my-take-while pred? (rest a-seq)))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (or (and (empty? a-seq)
               (not (empty? b-seq)))
          (and (not (empty? a-seq))
               (empty? b-seq)))
    false
    (if (and (empty? a-seq) (empty? b-seq))
      true
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn my-map [f seq-1 seq-2]
  (let [f1 (first seq-1)
        f2 (first seq-2)]
    (if (or (empty? seq-1) (empty? seq-2))
      '()
      (if (or (singleton? seq-1)
              (singleton? seq-2))
        (cons (f f1 f2) '())
        (cons (f f1 f2) (my-map f (rest seq-1) (rest seq-2)))))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (or (== n 1)
       (== n 2)) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotate [degrees a-seq]
  (let [num (count a-seq)]
    (if (== 0 (mod degrees num))
      (seq a-seq)
      (let [head (take degrees a-seq)
            tail (take-last (- num degrees) a-seq)]
        (concat tail head)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (conj a-seq '())
    (map (fn [deg] (rotate deg a-seq)) (range 0 (count a-seq)))))

(defn frequency-of [e a-seq]
  (count (filter (fn [x] (= x e)) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)]
      (assoc (my-frequencies-helper freqs (rest a-seq)) elem (frequency-of elem a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [pair (first a-map)
          key (first pair)
          num (second pair)]
      (concat (my-repeat num key) (un-frequencies (rest a-map))))))

(defn my-take [n cll]
  (if (or (empty? cll)
          (== n 0))
    '()
    (conj (my-take (- n 1) (rest cll)) (first cll))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (== n 0)
      coll
      (my-drop (- n 1) (rest coll)))))

(defn halve [a-seq]
  (let [num (count a-seq)
        first-half (int (/ num 2))]
    (conj (conj '() (my-drop first-half a-seq)) (my-take first-half a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (empty? a-seq) (not (empty? b-seq))) b-seq
     (and (not (empty? a-seq)) (empty? b-seq)) a-seq
     (< a b) (conj (seq-merge (rest a-seq) b-seq) a)
     (< b a) (conj (seq-merge a-seq (rest b-seq)) b)
     (= a b) (concat (conj (conj '() a) b) (seq-merge (rest a-seq) (rest b-seq)))
     :else '())))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    '()
    (if (singleton? a-seq)
      a-seq
      (let [halves (halve a-seq)
            a-half (first halves)
            b-half (second halves)]
        (seq-merge (merge-sort a-half) (merge-sort b-half))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])








