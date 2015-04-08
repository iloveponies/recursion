(ns recursion)

(defn product [coll]
  (if (==(count coll) 0)
    1
    (* (first coll)(product (rest coll)))))

(defn singleton? [coll]
  (==(count coll) 1))

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
  (if (<= (count seq-1)(count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))(cons (first a-seq)(my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))(cons (first a-seq)(my-take-while pred? (rest a-seq)))
   :else ()))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))(my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not(==(count a-seq)(count b-seq))) false
   (not(=(first a-seq)(first b-seq))) false
   (empty? (rest a-seq)) true
   :else (seq= (rest a-seq)(rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1) ()
   (empty? seq-2) ()
   :else (cons (f (first seq-1)(first seq-2))(my-map f (rest seq-1)(rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (if (== up-to 0)
    ()
    (cons (- up-to 1)(my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
    (map reverse (tails (reverse a-seq))))

(defn rotations-helper [n a-seq]
  (if (<= n 0)
    ()
    (cons a-seq (rotations-helper (- n 1) (concat(rest a-seq)(cons (first a-seq)()))))))

(defn rotations [a-seq]
    (if (empty? a-seq)
      (cons () ())
      (rotations-helper (count a-seq) a-seq)))


(defn my-frequencies-helper [freqs a-seq]
  (let [updated-freqs (if (contains? freqs (first a-seq))
                        (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1))
                        (assoc freqs (first a-seq) 1))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper updated-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (first (rest (first a-map))) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (<= n 0) ()
   (empty? coll) ()
   :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (<= n 0) coll
   (empty? coll) ()
   :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [len (int (/ (count a-seq) 2))]
    (cons (my-take len a-seq) (cons (my-drop len a-seq) ()))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq))(cons (first a-seq)(seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq)(seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [halved (halve a-seq)]
    (if (<(count a-seq) 2)
      a-seq
      (seq-merge (merge-sort(first halved)) (merge-sort(first (rest halved)))))))

(defn not-monotonic? [a-seq]
  (not(or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (let [monotonic (first(my-drop-while not-monotonic? (inits a-seq)))]
    (if (<= (count a-seq) 2)
      (cons a-seq ())
      (cons monotonic (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

