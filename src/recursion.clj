(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn recurse-max [max-fun a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max-fun (first a-seq) (recurse-max max-fun (rest a-seq))))))

(defn max-element [a-seq]
  (recurse-max max a-seq))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (>= c2 c1)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (recurse-max seq-max a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [first-elem (first a-seq)
          all-the-rest (rest a-seq)]
      (if (pred? first-elem)
        (cons first-elem (my-filter pred? all-the-rest))
        (my-filter pred? all-the-rest)))))

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
     '()
   (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
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
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (and ((complement empty?) a-seq)
             ((complement empty?) b-seq)
             (= (first a-seq) (first b-seq)))
      (seq= (rest a-seq) (rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0)
     1
   (= k 1)
     n
   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0)
     0
   (or (= n 1) (= n 2))
     1
   :else
     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn from-amount [from how-many a-seq]
  (->> a-seq (take (+ from how-many)) (drop from)))

(defn rotations [a-seq]
  (let [len (count a-seq)
        cycled-seq (cycle a-seq)]
    (if (< len 1)
      '(())
      (map (fn [index] (from-amount index len cycled-seq)) (range len)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-freq (get freqs elem)
          elem-count (if (nil? elem-freq)
                       1
                       (inc elem-freq))
          new-freqs (assoc freqs elem elem-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[elem times] (first a-map)]
      (concat (repeat times elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    (vector (my-take middle a-seq) (my-drop middle a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (let [num-to-insert (first a-seq)
          smaller (take-while (fn [x] (> num-to-insert x)) b-seq)
          larger (drop-while (fn [x] (>= num-to-insert x)) b-seq)
          new-b-seq (concat smaller (vector num-to-insert) larger)]
      (seq-merge (rest a-seq) new-b-seq))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (let [[a b] (halve a-seq)]
                 (vector (merge-sort a) (merge-sort b))))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq)
      '()
    (even? (count a-seq))
      (cons (take 2 a-seq) (split-into-monotonics (drop 2 a-seq)))
    :else
      (if (= (count a-seq) 1)
        (apply list a-seq)
        (cons (take 3 a-seq) (split-into-monotonics (drop 3 a-seq)))
       )))

(defn one-plus [frst others]
  (map (fn [other] (cons frst other)) others))

(defn permutations [a-set]
  (let [size (count a-set)]
    (cond
      (< size 1)
        '(())
      (= size 1)
        a-set
      (= size 2)
        (rotations a-set)
      :else
        (let [rots (rotations a-set)]
          (apply concat
                   (map (fn [rot]
                     (one-plus
                       (first rot)
                       (permutations (rest rot))))
                    rots))))))

(defn join [a-set b-set]
  (distinct (concat a-set b-set)))

; unefficient, runs out of memory with bigger than 8 elements
(defn pwset [a-set]
  (if (or (empty? a-set) (empty? (rest a-set)))
        #{a-set}
        (let [one-less-rots (map set (map rest (rotations a-set)))
              one-less-pws (apply concat (map pwset one-less-rots))
              ]
          (cons (apply hash-set a-set) (distinct (concat (join one-less-rots one-less-pws) #{#{}}))))))

(defn powerset [a-set]
  (pwset a-set))
