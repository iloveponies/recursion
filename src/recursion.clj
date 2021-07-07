(ns recursion)
(declare permutations)


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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max
      (first a-seq)
      (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if
      (pred? (first a-seq))
      (cons (first a-seq)
        (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (== elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
    (cons
      (first a-seq)
      (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    :else
      (and
        (== (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or
      (empty? seq-1)
      (empty? seq-2))
        '()
      :else
        (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (< n 2)
      n
    :else
      (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [n] (concat (drop n a-seq) (take n a-seq))) (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
      found (find freqs item)
      old_value (second found)
      helper (fn [value] (my-frequencies-helper (assoc freqs item value) (rest a-seq)))]
      (if found
        (helper (inc old_value))
        (helper 1)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[key cnt] (first a-map)]
      (concat
        (repeat cnt key)
        (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
    [(my-take x a-seq) (my-drop x a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if(< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (let [sq1 (first (halve a-seq))
      sq2 (second (halve a-seq))]
      (seq-merge (merge-sort sq1) (merge-sort sq2)))
    a-seq))

(defn monotonic [n a-seq]
  (if (= n (count a-seq))
    a-seq
    (let [test-seq (my-take (inc n) a-seq)
      is_inc? (apply <= test-seq)
      is_dec? (apply >= test-seq)]
    (if (or is_inc? is_dec?)
      (monotonic (inc n) a-seq)
      (my-take n a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [sq (monotonic 0 a-seq)
      left (my-drop (count sq) a-seq)]
      (cons sq (split-into-monotonics left)))))

(defn permutator [a-set b-set]
  (let [item (first a-set)
    m-rest (rest a-set)
    m-add (fn [x] (cons item x ))
    m-pred? (fn [x] (not (= item x)))]
    (if (empty? m-rest)
      (if (empty? (rest b-set))
        (list b-set)
        (map m-add (permutations (clojure.set/select m-pred? b-set))))
      (concat
        (map m-add (permutations (clojure.set/select m-pred? b-set)))
        (permutator m-rest b-set)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (permutator (into #{} a-set) (into #{} a-set ))))

(defn powersetter [tmp x]
  (if (empty? x)
    tmp
    (let [fst (first x)
      mcons (fn [x] (set (cons fst x)))
      fce (fn [x] (map mcons x))]
      (powersetter (concat tmp (fce tmp)) (rest x)))))

(defn powerset [a-set]
  (set (powersetter #{#{}} a-set)))
