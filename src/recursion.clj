(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn cmp-element [cmp a-seq]
  (let [fst (first a-seq)
        rst-1 (rest a-seq)
        snd (first rst-1)
        rst-2 (rest rst-1)
        cmp-pair (fn [a b] (if (cmp a b) a b))]
    (if (empty? rst-1)
        fst
        (cmp-element cmp (cons (cmp-pair fst snd) rst-2)))))

(defn max-element [a-seq]
  (cmp-element > a-seq))

(defn seq< [s-1 s-2]
  (cond
    (empty? s-1) true
    (empty? s-2) false
    :else (seq< (rest s-1) (rest s-2))))

(defn seq-max [seq-1 seq-2]
    (if (seq< seq-1 seq-2) seq-2 seq-1))

;(defn seq-max [seq-1 seq-2]
;  (let [seq< (fn seq< [s-1 s-2]
;               (cond
;                 (empty? s-1) true
;                 (empty? s-2) false
;                 :else (seq< (rest s-1) (rest s-2))
;                 ))]
;    (if (seq< seq-1 seq-2) seq-2 seq-1)))

(defn longest-sequence [a-seq]
  (cmp-element (complement seq<) a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst)
          (cons fst rst)
          rst))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
    (empty? a-seq) '()
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)
        a-fst (first a-seq)
        b-fst (first b-seq)
        a-rst (rest a-seq)
        b-rst (rest b-seq)]
  (cond
    (and a-empty b-empty) true
    (or a-empty b-empty) false
    (= a-fst b-fst) (seq= a-rst b-rst)
    :else false
    )))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations-helper [a-seq suffix]
  (if (empty? a-seq)
    ()
    (cons (concat a-seq suffix)
          (rotations-helper (rest a-seq) (conj suffix (first a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq [])))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)]
        (my-frequencies-helper (assoc freqs fst (inc (or (get freqs fst) 0) ))
                               (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  
  (if (empty? a-map)
    ()
    (let [[k v] (first a-map)
        a-rest (dissoc a-map k)]
    (concat (repeat v k) (un-frequencies a-rest)))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
  [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-fst (first a-seq) b-fst (first b-seq) ]
      (if (< a-fst b-fst)
        (cons a-fst (seq-merge (rest a-seq) b-seq))
        (cons b-fst (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (let [[head tail] (halve a-seq)]
    (cond
      (empty? head) a-seq
      (empty? tail) a-seq
      :else (seq-merge (merge-sort head) (merge-sort tail)))))

(defn monotonic? [a-seq]
  (or
    (empty? a-seq)
    (apply <= a-seq)
    (apply >= a-seq)
  ))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (let [is (inits a-seq)
            monotonic-is (my-take-while monotonic? is)
            longest-monotonic (last monotonic-is)
            nonmonotonic-part (my-drop (count longest-monotonic) a-seq)]
      (cons longest-monotonic (split-into-monotonics nonmonotonic-part)))))

(defn permutations-helper [a-seq head]
  (let [f (fn [i] 
            (let [prefix (my-take i a-seq)
                  element (nth a-seq i)
                  suffix (my-drop (inc i) a-seq)
                  leftovers (concat prefix suffix)
                  new-head (cons element head)
            ]
            ;new-head))
            (permutations-helper leftovers new-head)))
        ]

    (if (empty? a-seq)
        (list head)
        (apply concat (map f (my-range (count a-seq)))))))

(defn permutations [a-seq]
  (permutations-helper a-seq ()))


(defn get-sets [from to how-many]
  (let [get-single-set-and-recurse (fn [item] 
          (let [leftovers (disj from item)
                new-to (conj to item)]
            (if (zero? how-many)
                (list new-to)
                (get-sets leftovers new-to (dec how-many) )))) ]
    (apply concat (map get-single-set-and-recurse from))))

(defn powerset [a-seq]
  (let [a-set (set a-seq)
        subset-lengths (my-range (count a-set))
        all-subsets-of-length (fn [len] (get-sets a-set #{} len))
        ]
    (set (cons #{} (apply concat (map all-subsets-of-length subset-lengths))))))

