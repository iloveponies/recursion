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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [f (fn [n a-seq]
            (let [head (first a-seq)
                  tail (rest a-seq)]
              (if (empty? tail)
                n
                (if (> head n)
                  (recur head tail)
                  (recur n tail)))))]
    (if (empty? a-seq)
      nil
      (if (singleton? a-seq)
        (first a-seq)
        (f (first a-seq) (rest a-seq))))))

; I have no idea what you want here :(
(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (reduce #(if (> (count %2) (count %1)) %2 %1) (first a-seq) (rest a-seq)))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? head)
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      false
      (if (= elem head)
        true
        (sequence-contains? elem tail)))))

(defn my-take-while [pred? a-seq]
  (let [f (fn [pred? accum a-seq]
            (let [head (first a-seq)
                  tail (rest a-seq)]
              (if (empty? a-seq)
                (reverse accum)
                (if (pred? head)
                  (recur pred? (cons head accum) tail)
                  (reverse accum)))))]
        (if (empty? a-seq)
          a-seq
          (f (pred? '() a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? head)
        (cons head (my-take-while pred? tail))))))

(defn my-drop-while [pred? a-seq]
  [:-])

(defn seq= [a-seq b-seq]
  (let [ahead (first a-seq)
        atail (rest a-seq)
        bhead (first b-seq)
        btail (rest b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (empty? a-seq) false
      (empty? b-seq) false
      (= ahead bhead) (seq= atail btail)
      :else false)))



(defn my-map [f seq-1 seq-2]
  (let [head1 (first seq-1)
        tail1 (rest seq-1)
        head2 (first seq-2)
        tail2 (rest seq-2)]
    (if (and head1 head2)
      (cons (f head1 head2) (my-map f tail1 tail2))
      '())))

(defn power [n k]
  (cond
    (= 0 k) 1
    (= 1 k) n
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= 0 up-to) '()
    (= 1 up-to) '(0)
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (not (empty? a-seq))
    (cons a-seq (tails (rest a-seq)))
    [a-seq]))


(defn inits [a-seq]
  nil)


(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (let [f (fn [n accum items]
            (if (= 0 n)
              (reverse accum)
              (recur (- n 1) (cons (first items) accum) (rest items))))]
    (if (> n (count coll))
      (f (count coll) '() coll)
      (f n '() coll))))


(defn my-drop [n coll]
  (let [f (fn [n accum items]
            (if (> n 0)
              (recur (- n 1) accum (rest items))
              (if (empty? items)
                (reverse accum)
                (recur n (cons (first items) accum) (rest items)))))]
    (f n '() coll)))

(defn halve [a-seq]
  (let [len (int (/ (count a-seq) 2))]
    (vector (my-take len a-seq) (my-drop len a-seq))))

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

