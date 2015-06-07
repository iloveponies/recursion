(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (let [next-up-to (- up-to 1)]
    (if (< next-up-to 0)
      ()
      (cons next-up-to (my-range next-up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [seq-tails (tails a-seq)
        seq-inits (inits a-seq)]
    (if (empty? a-seq)
      (cons a-seq ())
      (filter (complement empty?) (map (fn [b-seq c-seq]
                                         (if (empty? b-seq)
                                           ()
                                           (concat b-seq c-seq)))
                                       seq-tails seq-inits)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-in-seq (first a-seq)
          current-freq (freqs current-in-seq)
          new-freq (if (= nil current-freq) 1 (inc current-freq))
          updated-freqs (assoc freqs current-in-seq new-freq)]
      (my-frequencies-helper updated-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [first-entry (first a-map)]
      (concat (repeat (val first-entry) (key first-entry))
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll) ()
   (<= n 0) ()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (cond
   (empty? coll) ()
   (<= n 0) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    (cons (my-take midpoint a-seq) (vector (my-drop midpoint a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [first-a (first a-seq)
           first-b (first b-seq)]
       (if (<= first-a first-b)
         (cons first-a (seq-merge (rest a-seq) b-seq))
         (cons first-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
   (<= 0 (count a-seq) 1) a-seq
   :else (let [split-seq (halve a-seq)
               first-half (first split-seq)
               second-half (second split-seq)]
           (seq-merge
            (merge-sort first-half)
            (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic? (fn [b-seq]
                       (if (empty? b-seq)
                         ()
                         (or (apply <= b-seq) (apply >= b-seq))))
          largest-found-monotonic (last (take-while monotonic? (inits a-seq)))
          rest-of-seq (drop (count largest-found-monotonic) a-seq)]
      (cons largest-found-monotonic (split-into-monotonics rest-of-seq)))))

(defn permutations [a-set]
  (cond
   (empty? a-set) ()
   (== 1 (count a-set)) a-set))

(defn powerset [a-set]
  [:-])

