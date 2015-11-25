(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (= nil (second (set coll))) (not (empty? coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))
  

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [elem (first a-seq)
          rest-seq (rest a-seq)]
    (if (pred? elem)
      (cons elem (my-filter pred? rest-seq))
      (my-filter pred? rest-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))
      
(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      ()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))) 
      

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
 (cond
   (or (= n 0) (= n 1))
     n
   :else
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [i] (let [splitted (split-at i a-seq)]
                   (concat (get splitted 1) (get splitted 0))))
         (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          freqs-elem (get freqs elem)]
     (my-frequencies-helper 
      (assoc freqs elem 
             (if (nil? freqs-elem) 1 (inc freqs-elem)))
      (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [keyvalue] (repeat (second keyvalue) (first keyvalue))) a-map)))

(defn my-take [n coll]
  (first (split-at n coll)))

(defn my-drop [n coll]
  (second (split-at n coll)))

(defn halve [a-seq]
  (split-at (int (/ (count a-seq ) 2)) a-seq))

(defn seq-merge [a-seq b-seq]
  (letfn [(merge-to [joined a b]
          (cond
            (and (empty? a) (empty? b))
                 (reverse joined)
            (or (empty? b) (and (not (empty? a)) (<= (first a) (first b))))
                (merge-to (cons (first a) joined) (rest a) b)
            :else
                (merge-to (cons (first b) joined) (rest b) a)))]
    (merge-to '() a-seq b-seq)))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort (first (halve a-seq)))
                 (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

