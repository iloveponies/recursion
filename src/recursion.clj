(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
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
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
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
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (list)
    (conj (my-map f (rest seq-1) (rest seq-2)) (f (first seq-1) (first seq-2)))))
    

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    (list)
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))
  
(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (conj (tails (rest a-seq)) (apply list a-seq))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (< (count a-seq) 2)
    (list (apply list a-seq))
    (rest (map concat 
                 (tails a-seq)
                 (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (nil? (freqs (first a-seq))) (my-frequencies-helper 
                                   (assoc freqs (first a-seq) 1) (rest a-seq)) 
    :else (my-frequencies-helper 
            (assoc freqs (first a-seq) (inc (freqs (first a-seq)))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (list)
    (let [[x n] (first a-map)]
      (concat (un-frequencies (rest a-map)) (repeat n x)))))

(defn my-take [n coll]
  (cond
    (zero? n) ()
    (> n (count coll)) coll
    :else (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (cond
    (zero? n) coll
    (> n (count coll)) ()
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq) (my-drop mid a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq)) 
            (conj
              (seq-merge (rest a-seq) b-seq)
              (first a-seq))
            (conj
              (seq-merge a-seq (rest b-seq))
              (first b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[top-half bottom-half] (halve a-seq)]     
      (seq-merge
        (merge-sort top-half)
        (merge-sort bottom-half)))))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (< (count a-seq) 1)
    ()
    (let [ls (longest-sequence
               (take-while monotonic? (rest (inits a-seq))))]
      (conj (split-into-monotonics (drop (count ls) a-seq)) ls))))

(defn permute [done-seq a-seq]
  (if (empty? a-seq)
    (list done-seq)
    (apply concat (map 
                    (partial permute (conj done-seq (first a-seq))) 
                      (rotations (rest a-seq))))))

 (defn permutations [a-set]
  (apply concat (map (partial permute ()) (rotations a-set))))
 
(defn powerset [a-set]
  (set (map set (apply concat (map inits (permutations a-set))))))
  
