(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (let [tail (rest coll)]
    (if (empty? tail)
      (first coll)
      (my-last tail))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max
            (first a-seq)
            (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (if (<= len-1 len-2) seq-2 seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? fst) (cons fst (my-filter pred? tail))
      :else (my-filter pred? tail))))

(defn sequence-contains? [elem a-seq]
  (let [fst (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) false
      (= fst elem) true
      :else (sequence-contains? elem tail))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? fst) (cons fst (my-take-while pred? tail))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (let [fst (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (pred? fst) (my-drop-while pred? tail)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [a-empty? (empty? a-seq)
        b-empty? (empty? b-seq)]
    (cond
      (or a-empty? b-empty?) (and a-empty? b-empty?)
      (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
      :else false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [new-val (f (first seq-1) (first seq-2))]
      (cons new-val (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (or (= n 1) (= n 0))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons
      what-to-repeat
      (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0) '() (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [init (inits a-seq)
        tail (reverse (tails a-seq))
        init-tail-combos (map vector tail init)
        concat-vecs (fn [x] (apply concat x))]
    (distinct (map concat-vecs  init-tail-combos))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          tail (rest a-seq)
          new-frq (+ (get freqs fst 0) 1)
          freqs (assoc freqs fst new-frq)]
      (my-frequencies-helper freqs tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[chr reps] (first a-map)
          tail (rest a-map)]
      (concat (repeat reps chr) (un-frequencies tail)))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [idx (int (/ (count a-seq) 2))]
    [(my-take idx a-seq) (my-drop idx a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (concat a-seq b-seq)
    (let [a-fst (first a-seq)
          b-fst (first b-seq)]
      (if (< a-fst b-fst)
        (cons a-fst (seq-merge (rest a-seq) b-seq))
        (cons b-fst (seq-merge (rest b-seq) a-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[left, right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [seqs (reverse (inits a-seq))
          monos (take-while monotonic? seqs)
          n-elems (- (count monos) 1)
          nxt (first (reverse monos))]
      (cons nxt (split-into-monotonics (drop n-elems a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [permutate (fn [a-rot]
                      (let [[fst & tail] a-rot
                            cons-fst (fn [y] (cons fst y))]
                        (map cons-fst (permutations tail))))]
      (mapcat permutate (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [rots (rotations a-set)
          dropthing (fn [a-rot] (powerset (set(rest a-rot))))
          the-rest (set (mapcat dropthing rots))]
      (clojure.set/union #{(set a-set)} the-rest))))



