(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (let [tail (rest coll)]
      (empty? tail))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [head (first a-seq)
                next (first (rest a-seq))
                tail (rest (rest a-seq))]
            (max-element (cons (max head next) tail)))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [head (first a-seq)
                next (first (rest a-seq))
                tail (rest (rest a-seq))]
            (longest-sequence (cons (seq-max head next) tail)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) (list)
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) (list)
      (pred? head) (cons head (my-take-while pred? tail))
      :else (list))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) (list)
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (list)
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    (list)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    (list)
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq (list))
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (loop [coll (seq a-seq)
         acc ()
         len (count coll)]
    (cond
      (and (nil? coll)
           (= 0 (count acc))) (list acc)
      (= len (count acc)) acc
      :else (recur (concat (rest coll) (list (first coll)))
             (cons coll acc)
             len))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          tail (rest a-seq)]
      (if (not (contains? freqs head))
        (my-frequencies-helper (assoc freqs head 1) tail)
        (my-frequencies-helper (assoc freqs head (inc (get freqs head))) tail)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (loop [coll a-map
         head (first a-map)
         tail (rest a-map)
         acc ()]
    (if (empty? coll)
      acc
      (recur tail
             (first tail)
             (rest tail)
             (concat acc (my-repeat (nth head 1) (first head)))))))

(defn my-take [n coll]
  (loop [num n
         a-seq (seq coll)
         acc ()]
    (if (or (= num 0)
            (nil? a-seq))
      (reverse acc)
      (recur (dec num)
             (seq (rest a-seq))
             (cons (first a-seq) acc)))))

(defn my-drop [n coll]
  (loop [num n
         a-seq (seq coll)]
    (cond
      (nil? a-seq) ()
      (= num 0) a-seq
      :else (recur (dec num) (seq (rest a-seq))))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (conj []
          (my-take n a-seq)
          (my-drop n a-seq))))

(defn seq-merge [a-seq b-seq]
  (loop [a-coll (seq a-seq)
         b-coll (seq b-seq)
         acc []]
    (cond
      (nil? a-coll) (seq (concat acc b-coll))
      (nil? b-coll) (seq (concat acc a-coll))
      (< (first a-coll) (first b-coll)) (recur (seq (rest a-coll))
                                               b-coll
                                               (conj acc (first a-coll)))
      :else (recur a-coll
                   (seq (rest b-coll))
                   (conj acc (first b-coll))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

