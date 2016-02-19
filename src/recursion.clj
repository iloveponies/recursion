(ns recursion)
(use 'recursion)

(defn product [coll]
  (reduce * coll))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (loop [head (first coll)
         tail (rest coll)]
    (if (empty? tail)
      head
      (recur (first tail) (rest tail)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
   nil
   (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= elem (first a-seq))
    true
    :else
    (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first  seq-1) (first  seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 1)
    n
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (dec n))
                 (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat
                          (dec how-many-times)
                          what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (inits (rest a-seq)))))

(defn tails [a-seq]
  (inits (reverse a-seq)))

(defn rotate 
  "returns the sequence with it's head moved to the end"
    [a-seq]
    (concat (rest a-seq) (list (first a-seq)) ))

(defn rotations-with-iterator [a-seq n]
  (if (= n 0)
    (take n a-seq)
    (concat (list a-seq) (take n (rotations-with-iterator (rotate a-seq) (dec n))))))

(defn rotations [a-seq]
  (rotations-with-iterator a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if  (empty? a-seq)
    freqs
    (let [head (first a-seq)
          tail (rest a-seq)
          new-freqs (assoc freqs head (inc (or (get freqs head) 0)))
          ]
      (my-frequencies-helper new-freqs tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-freq-helper [coll a-map]
  (if (empty? a-map)
    coll
    (let [head (first a-map)
          tail (rest a-map)
          key (first head)
          val (second head)]
      (un-freq-helper (concat coll (repeat val key))
                      tail))))

(defn un-frequencies [a-map]
  (un-freq-helper [] a-map))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    []
    (cons (first coll)
            (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (concat (list (my-take half a-seq)) (list (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

(defn my-lt
  "less-than operation that can compare nil without exploding"
  [n m]
  (cond (nil? n) false
        (nil? m) true
        :else (< n m)))

(defn seq-merge-helper [a-seq b-seq acc]
  (if (and (empty? a-seq) (empty? b-seq))
    acc
    (let [a (first a-seq)
          b (first b-seq)]
      (cond (my-lt a b)
            (seq-merge-helper (rest a-seq) b-seq (conj acc a))
            :else
            (seq-merge-helper a-seq (rest b-seq) (conj acc b))))))

(defn merge-sort [a-seq]
  (let [half-a (first (halve a-seq))
        half-b (second (halve a-seq))]
    (seq-merge
     (if (< (count half-a) 2)
       half-a
       (merge-sort half-a))
     (if (< (count half-b) 2)
       half-b
       (merge-sort half-b)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

