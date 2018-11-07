(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (or (empty? coll) (not (empty? (rest coll)))) false true)) 

(defn my-last [coll]
  (if (or (empty? coll)  (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq)  (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2)) 

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq)  (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [remainder (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq))
        (cons (first a-seq) remainder)
        remainder))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (== elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (drop-last a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [lenA (count a-seq)]
      (take lenA (partition lenA 1 (cycle a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [firstA (first a-seq)
          new-freqs (assoc freqs firstA (+ 1 (get freqs firstA 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)) 

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (<= n 0) '()
    (>= n (count coll)) coll
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [lenA (count a-seq)
        firstH (int (/ lenA 2))]
    (cons (my-take firstH a-seq) (cons (my-drop firstH  a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
  a-seq
  (let [[firstH, lastH] (halve a-seq)
        sortedFirstH (merge-sort firstH)
        sortedLastH (merge-sort lastH)]
    (seq-merge sortedFirstH sortedLastH))))
(defn isSorted [a-seq compareOp]
  (if (or (empty? a-seq) (singleton? a-seq))
    true
    (and (compareOp (first a-seq) (first (rest a-seq)))
         (isSorted (rest a-seq) compareOp))))
(defn isMonotonic [a-seq]
  (or
    (isSorted a-seq <=)
    (isSorted a-seq >)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [firstSeq (my-last (take-while isMonotonic (reverse (inits a-seq))))]
      (cons firstSeq
            (split-into-monotonics (drop (count firstSeq) a-seq))))))

(defn multipleAppend [elem a-sets]
  (if (empty? a-sets)
    '()
    (cons (cons elem (first a-sets)) (multipleAppend elem (rest a-sets)))))

(declare permutations)

(defn permuteOne [a-set]
  (if (<= (count a-set) 2)
    a-set
    (let [firstE (first a-set)
          restP (permutations (rest a-set))]
      (multipleAppend firstE restP))))

(defn permutations [a-set]
  (if  (empty? a-set)
    '(())
    (let [allRotations (rotations a-set)
          allPerms (map permuteOne allRotations)]
      (if (<= (count a-set) 2)
        allPerms
        (apply concat allPerms)))))

(defn powerset [a-set]
  (cond
    (empty? a-set) '(())
    :else
    (let [firstE (first a-set)
          powerRest (powerset (rest a-set))
          firstPower (multipleAppend firstE powerRest)]
      (concat powerRest firstPower))))

