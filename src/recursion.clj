(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
        (cond 
          (empty? a-seq) nil
          (empty? tail) head
          :else (seq-max head (longest-sequence tail)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) 
    []
    (let [item (first a-seq)
          filtered-rest (my-filter pred? (rest a-seq))]
          (if (pred? item)
            (cons item filtered-rest)
            filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) 
    false
    (let [head (first a-seq)
          tail (rest a-seq)]
            (if (= head elem)
              true
              (sequence-contains? elem tail)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
          (if (pred? head)
            (cons head (my-take-while pred? tail))
            []))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
          (if (pred? head)
            (my-drop-while pred? tail)
            a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ 
            (fib (- n 2))
            (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    []))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          freq (get freqs item)
          new-freq (if (= freq nil) 1 (inc freq))]
          (my-frequencies-helper (assoc freqs item new-freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [pair (first a-map)]
          (concat (repeat (second pair) (first pair)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(take half a-seq) (drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-empty? (empty? a-seq)
        b-empty? (empty? b-seq)]
        (cond
          a-empty? b-seq
          b-empty? a-seq
          :else (let [a-head (first a-seq)
                      b-head (first b-seq)
                      a-tail (rest a-seq)
                      b-tail (rest b-seq)]
                      (if (< a-head b-head)
                        (cons a-head (seq-merge a-tail b-seq))
                        (cons b-head (seq-merge a-seq b-tail)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [halves (halve a-seq)]
          (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn cut-first-monotonic [a-seq]
  (let [diff-seq (map (fn [curr prev] [curr (- curr prev)]) a-seq (cons (first a-seq) a-seq))
        select-func (fn [pair] (not (< (* (second pair) (second (second diff-seq))) 0)))]
        [(map first (take-while select-func diff-seq))
        (map first (drop-while select-func diff-seq))]))

(defn split-into-monotonics [a-seq]
  (let [[first-monotonic rest-seq] (cut-first-monotonic a-seq)]
        (if (empty? rest-seq)
          [first-monotonic]
          (cons first-monotonic (split-into-monotonics rest-seq)))))

(defn insert-everywhere-helper [value a-set place]
  (if (= place 0)
    [(cons value a-set)]
    (let [item (concat (take place a-set) (cons value (drop place a-set)))]
          (cons item (insert-everywhere-helper value a-set (dec place))))))

(defn insert-everywhere [value a-set]
  (insert-everywhere-helper value a-set (count a-set)))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (apply concat (map (fn [permutation] (insert-everywhere (first a-set) permutation)) (permutations (rest a-set))))))

(defn get-subset [a-set n]
  (if (= n 0)
    []
    (let [head (first a-set)
          tail (get-subset (rest a-set) (quot n 2))]
          (if (= (mod n 2) 1)
            (cons head tail)
            tail))))

(defn powerset-helper [a-set n]
  (if (< n 0)
    []
    (cons (set (get-subset a-set n)) (powerset-helper a-set (dec n)))))

(defn powerset [a-set]
  (set (powerset-helper a-set (dec (power 2 (count a-set))))))