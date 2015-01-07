(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [car (first a-seq)
          cdr (rest a-seq)]
      (max car
           (if (empty? cdr)
             0
             (max-element cdr))))))

(defn sum [coll]
  (if (empty? coll)
    0
    (+ (first coll)
       (sum (rest coll)))))

(defn seq-max [seq-1 seq-2]
  (let [seq-1-count (count seq-1)
        seq-2-count (count seq-2)]
    (if (< seq-1-count seq-2-count)
      seq-2
      (if (> seq-1-count seq-2-count)
        seq-1
        (if (< (max-element seq-2) (max-element seq-1))
          seq-1
          seq-2)))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [car (first a-seq)
          cdrf (my-filter pred? (rest a-seq))]
      (if (pred? car)
        (cons car cdrf)
        cdrf))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons (first a-seq) (my-drop-while (fn [x] false) (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (count a-seq) (count b-seq))) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq
          (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (let [a (rest (tails a-seq))
          b (rest (reverse (inits a-seq)))]
      (map concat a b))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs
             (first a-seq)
             (+ 1 (let [val (get freqs (first a-seq))]
                    (if (nil? val)
                      0
                      val))))
      (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [x] (let [[k n] x] (repeat n k))) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop-helper [m n coll]
  (cond
    (empty? coll) '()
    (< m n) (my-drop-helper (+ m 1) n (rest coll))
    :else (cons (first coll) (my-drop-helper m n (rest coll)))))
(defn my-drop [n coll]
    (my-drop-helper 0 n coll))

(defn halve [a-seq]
  (let [mid-pt (int (/ (count a-seq) 2))]
    (vector
      (my-take mid-pt a-seq)
      (my-drop mid-pt a-seq))))

(defn seq-merge [a-seq b-seq]
   (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
     (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     (< (first b-seq) (first a-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
     (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else (cons (first a-seq) (cons (first b-seq) (seq-merge (rest a-seq) (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (empty? (rest a-seq)) a-seq
    :else (let [[s1 s2] (halve a-seq)]
            (seq-merge
              (merge-sort s1)
              (merge-sort s2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

