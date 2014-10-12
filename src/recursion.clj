(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    []
    (and
      (not (nil? (first coll)))
      (empty? (rest coll))
      (not (nil? (rest coll))))))


(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (= (singleton? a-seq) []) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (= (singleton? a-seq) []) nil
   (empty? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (true? (pred? (first a-seq)))(cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not= (count a-seq) (count b-seq)) false
   (empty? (and a-seq b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
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
          (fib (dec n))
          (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () (seq a-seq))
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () (seq a-seq))
    (reverse (cons (seq a-seq) (tails (rest a-seq))))))

(defn rotations [a-seq]
  (defn help [num seq]
    (if (= num 0)
      seq
      (concat (help (dec num) (concat (rest seq) (vector (first seq)))) seq)))
  ; divide rotations to groups size of count of original seq
  (distinct (partition (count a-seq) (help (count a-seq) a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [inc-elem (if (nil? (freqs (first a-seq)))
                     (assoc freqs (first a-seq) 1)
                     (assoc freqs (first a-seq) (inc (freqs (first a-seq)))))]
    (my-frequencies-helper inc-elem (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (first (vals a-map)) (first  (keys a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or
       (= n 0)
       (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) ()
   (= n 0) (cons (first coll) (my-drop 0 (rest coll)))
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
 (let [middle-index (int (/ (count a-seq) 2))]
   (vector (my-take middle-index a-seq) (my-drop middle-index a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (cond
   (= (count a-seq) 1) a-seq
   (= (count a-seq) 2) (seq-merge (vector (first a-seq)) (vector (second a-seq)))
   :else (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves))
                 (merge-sort (second halves))))))
;;   (if (= (count a-seq) 2)
;;     (seq-merge (vector (first a-seq)) (vector (second a-seq)))
;;     (let [halves (halve a-seq)]
;;       (seq-merge (merge-sort (first halves))
;;                  (merge-sort (second halves))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

