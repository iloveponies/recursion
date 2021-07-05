(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (== 0 (count a-seq))
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [item (first a-seq)
          predicated (my-filter pred? (rest a-seq))]
      (if (pred? item)
        (cons item predicated)
        predicated))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (let [item (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? item) (cons item (my-take-while pred? (rest a-seq)))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)]
    (cond
      (and a-empty b-empty) true
      (or a-empty b-empty) false
      (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
      :else false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (let [decreased-up-to (dec up-to)]
    (if (> 0 decreased-up-to)
      '()
      (cons decreased-up-to (my-range decreased-up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [last-rotation n]
  (let [new-rotation (concat (rest last-rotation) (vector (first last-rotation)))]
    (if (< 1 n)
      (concat [new-rotation] (rotations-helper new-rotation (dec n)))
      [new-rotation])))

(defn rotations [a-seq]
  (if (> 1 (count a-seq))
    '(())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          new-count (inc (if (contains? freqs item)
                           (get freqs item)
                           0))]
      (my-frequencies-helper (assoc freqs item new-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [first-item (first a-map)
        repeated-item (repeat (second first-item) (first first-item))]
    (if (== 1 (count a-map))
      repeated-item
      (concat repeated-item (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (> 1 n) (empty? coll))
    '()
    (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (> 1 n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid-length (int (/ (count a-seq) 2))]
    (concat [(my-take mid-length a-seq)] [(my-drop mid-length a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)
        a-item (first a-seq)
        b-item (first b-seq)]
    (cond
      (and a-empty b-empty) '()
      a-empty (concat [b-item] (seq-merge a-seq (rest b-seq)))
      b-empty (concat [a-item] (seq-merge (rest a-seq) b-seq))
      :else (if (< a-item b-item)
              (concat [a-item] (seq-merge (rest a-seq) b-seq))
              (concat [b-item] (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (let [seq-len (count a-seq)
        halved (halve a-seq)]
    (cond
      (== 1 seq-len) a-seq
      (== 0 seq-len) '()
      :else (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn rising-monotonic? [a-seq]
  (cond
    (empty? a-seq) true
    (== 1 (count a-seq)) true
    (<= (first a-seq) (second a-seq)) (rising-monotonic? (rest a-seq))
    :else false))

(defn decreasing-monotonic? [a-seq]
  (rising-monotonic? (reverse a-seq)))

(defn monotonic? [a-seq]
  (or (rising-monotonic? a-seq) (decreasing-monotonic? a-seq)))

(defn split-into-monotonics [a-seq]
  (let [init-seqs (reverse (inits a-seq))
        init-split (last (take-while monotonic? init-seqs))
        remainder (drop (count init-split) a-seq)]
    (if (empty? remainder)
      [init-split]
      (concat [init-split] (split-into-monotonics remainder)))))

(defn swap [a-seq i j]
  (if (and (< i (count a-seq)) (< j (count a-seq)))
    (let [a-vec (apply vector a-seq)
          i-item (get a-vec i)
          j-item (get a-vec j)]
      (assoc (assoc a-vec i j-item) j i-item))
    a-seq))

;Really dumb and slow permutations method
(defn perm-helper [a-seq result i j]
  (let [length (dec (count a-seq))]
    (if (== length i)
      result
      (let [swapped (seq (swap a-seq i j))
            new-result (set (conj result swapped))
            new-i (inc i)]
        (if (== j length)
          (perm-helper swapped new-result new-i 0)
          (let [original-swap (perm-helper a-seq new-result i (inc j))
                new-swap (perm-helper swapped new-result i (inc j))]
            (concat (seq new-result) original-swap new-swap)))))))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (seq (set (perm-helper a-seq '() 0 0)))))

(defn union-all [a-set v]
  (map (fn [x] (clojure.set/union x #{v})) a-set))

(defn powerset [a-seq]
  (let [length (count a-seq)]
    (cond
      (== 0 length) #{#{}}
      (== 1 length) #{#{} #{(first a-seq)}}
      :else (let [subset (powerset (rest a-seq))
                  concat-subset (union-all subset (first a-seq))]
              (clojure.set/union subset concat-subset)))))
