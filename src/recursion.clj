(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (= () (rest coll))))

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
    (if (not (pred? (first a-seq)))
      (my-filter pred? (rest a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (not (= elem (first a-seq)))        ; doesn't match first element
     (sequence-contains? elem (rest a-seq)) ; keep looking through rest of seq
   :else                               ; first element matches
     true))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (not (pred? (first a-seq)))
      a-seq
      (my-drop-while pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true    ; both empty
    (empty? b-seq) false
    (not (= (first a-seq) (first b-seq))) false ; values don't match
    :else (seq= (rest a-seq) (rest b-seq))))    ; value for rest of seqs

(defn my-map [f seq-1 seq-2]
  (if (empty? seq-2)
    ()
    (let [first-elem (f (first seq-1) (first seq-2))]
      (if (singleton? seq-1)
        [first-elem]
        (cons first-elem (my-map f (rest seq-1) (rest seq-2)))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) 
    ()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons
      (seq a-seq)
      (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (quote (()))
    (let [double (concat a-seq a-seq)
          indices (range (count a-seq))
          map-pred 
          (fn [idx] (take (count a-seq) 
                          (drop-while #(not (= (a-seq idx) %)) double)))]
      (map map-pred indices))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [cur-key (first a-seq)
          cur-freq (freqs cur-key)
          new-freq (if (nil? cur-freq)
                     1
                     (inc cur-freq))]
      (my-frequencies-helper (assoc freqs cur-key new-freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [num-repeats (val (first a-map))
          elem (key (first a-map))]
      (concat (repeat num-repeats elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    ()
    (if (= n (count coll))
      coll
      (my-drop n (rest coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(take half a-seq) 
     (drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) []
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) ;(println ">=")
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (>= (first a-seq) (first b-seq)) ;(println "<")
          (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
        :else (println "oops")))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [halves (halve a-seq)
          front (first halves)
          back (second halves)]
      (seq-merge (merge-sort front) (merge-sort back)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    false
    (or (apply >= a-seq)
        (apply <= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [candidates (inits a-seq)
          best (last (filter monotonic? candidates))
          remaining (take-last (- (count a-seq) (count best)) a-seq)]
        (cons best (split-into-monotonics remaining)))))

(defn perm-helper [a-set acc]
  (if (empty? a-set)
    (list acc)
    (apply concat (map #(perm-helper 
                         (remove (fn [x] (= x %)) a-set) 
                         (cons % acc)) 
                       a-set))))

(defn permutations [a-set]
  (perm-helper a-set '()))

(permutations #{})
(permutations #{1 5})
(permutations #{1 5 3})

(defn powerset [a-set]
  [:-])

