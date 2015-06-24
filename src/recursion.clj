(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (let [dem (rest coll)]
      (empty? dem))))

(defn my-last [coll]
  (if (or (= nil (first coll)) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (> (first a-seq) (my-last a-seq))
        (max-element (disj (my-last a-seq)))
        (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (if (= (my-last a-seq) (seq-max (first a-seq) (my-last a-seq)))
           (longest-sequence (rest a-seq))
           (longest-sequence (disj (my-last a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons
       (first a-seq)
       (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not (== (count a-seq) (count b-seq))) false
   (not (== (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    ()
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    ()
    (cons
     (dec up-to)
     (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq)
          (inits (drop-last a-seq)))))



(defn rotations [a-seq]
  (let [rotate (fn [b-seq] (concat (rest b-seq) [(first b-seq)]))]
    (cond
     (empty? a-seq) [()]
     (not (coll? (first a-seq))) (rotations (cons (seq a-seq)
                                           [(rotate a-seq)]))
     (== (count a-seq) (count (first a-seq))) a-seq
     :else (let [vika (last a-seq)]
           (rotations (concat a-seq
                              [(rotate vika)]))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [dis (first a-seq)]
      (if (contains? freqs dis)
        (my-frequencies-helper (assoc freqs dis (inc (get freqs dis))) (rest a-seq))
        (my-frequencies-helper (assoc freqs dis 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [eka (first a-map)]
      (concat (repeat (second eka) (first eka))
              (un-frequencies (dissoc a-map (first eka)))))))

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
    ()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (>= 0 n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    (concat [(my-take middle a-seq)]
            [(my-drop middle a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) ()
        (and (not (empty? a-seq)) (empty? b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (and (empty? a-seq) (not (empty? b-seq))) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves))
                 (merge-sort (last halves))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

