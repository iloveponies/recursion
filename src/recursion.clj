(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll))
           (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (let [head (first coll)
        tail (rest coll)]
    (if (empty? tail)
      head
      (my-last tail))))

(defn max-element [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond (empty? a-seq) nil
          (empty? tail) head
          :else (max head
                     (max-element tail)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond (empty? a-seq)
            a-seq
          (pred? head)
            (cons head
                  (my-filter pred? tail))
          :else
            (my-filter pred? tail))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq)
          false
        (= (first a-seq) elem)
          true
        :else
          (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq)
          '()
        (pred? (first a-seq))
          (cons (first a-seq)
                (my-take-while pred? (rest a-seq)))
        :else
          '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq)
          '()
        (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
        :else
          (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (pos? up-to)
    (cons (dec up-to)
          (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons '() nil)
   :else (cons (seq a-seq)
               (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [tails1 (tails (reverse a-seq))
        tails2 (map reverse tails1)
        tails3 (reverse tails2)]
    tails3))

(defn rotate [a-seq]
  (concat (rest a-seq) [(first a-seq)]))

(defn rotations-helper [a-seq orig-seq]
  (if (= a-seq orig-seq)
    (cons orig-seq nil)
    (cons a-seq
          (rotations-helper (rotate a-seq) orig-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() nil)
    (rotations-helper (rotate a-seq) (seq a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          old-count (freqs key)
          new-count (if (nil? old-count)
                      1
                      (inc old-count))]
      (my-frequencies-helper (assoc freqs key new-count)
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [s a-map]
  (if (empty? a-map)
    s
    (let [[key n] (first a-map)
          n-key-seq (repeat n key)]
      (un-frequencies-helper (concat s n-key-seq) (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

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

