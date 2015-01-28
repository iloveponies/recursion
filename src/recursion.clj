(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (nil? a-seq) nil
    (empty? a-seq) nil
    :else (apply max (set a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
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
   (empty? a-seq) []
   (pred? (first a-seq))
   (cons (first a-seq)
         (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or
    (or (empty? a-seq) (empty? b-seq))
    (not (= (first a-seq) (first b-seq)))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) []
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n))
            (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat)))
)

(defn my-range [up-to]
  (if (>= 0 up-to) []
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    '(())
    (cons (reverse (into () a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [head-list tail-list]
  (cond
    (empty? tail-list) []
    :else (cons (concat tail-list head-list) (rotations-helper (concat head-list [(first tail-list)]) (rest tail-list)))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) [[]]
    :else (rotations-helper [] a-seq)))

(defn my-frequencies-helper [freqs a-seq]
   (if (empty? a-seq)
        freqs
        (let [first-item (first a-seq)
              curre-value (get freqs first-item)
              new-fags (if (nil? curre-value)
                         (assoc freqs first-item 1)
                         (assoc freqs first-item (inc curre-value)))]
          (my-frequencies-helper new-fags (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat #(repeat (second %) (first %)) (seq a-map)))

(defn my-take [n coll]
  (cond
    (or (zero? n) (empty? coll)) []
    :else (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (cond
    (or (zero? n) (empty? coll)) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    [(my-take h a-seq) (my-drop h a-seq)]))

(defn seq-merge-helper [a b completed]
  (cond
    (empty? a) (seq-merge-helper (rest a) (rest b) (conj completed (first b)))
    (empty? b) (seq-merge-helper (rest a) (rest b) (conj completed (first a)))
    (< (first a) (first b)) (seq-merge-helper (rest a) b (conj completed (first a)))
    (> (first a) (first b)) (seq-merge-helper a (rest b) (conj completed (first b)))
    :else completed))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

