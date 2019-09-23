(ns recursion)


(defn product [coll]
  (cond
   (empty? coll) 1
   :else    (* (first coll) (product (rest coll)))
  ))



(defn singleton? [coll]
  (cond
   (empty? coll) false
   :else (empty? (rest coll))
  ))


(defn my-last [coll]
  (cond
   (empty? coll)      nil
   (singleton? coll) (first coll)
   :else             (my-last (rest coll))
  ))


(defn max-element [a-seq]
  (cond
   (empty? a-seq)     nil
   (singleton? a-seq) (first a-seq)
   :else              (max (first a-seq) (max-element (rest a-seq)))
  ))



(defn seq-max [seq-1 seq-2]
  (let [seq1L (count seq-1)
        seq2L (count seq-2)]
  (cond
   (> seq1L seq2L) seq-1
   :else seq-2
  )))


(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq)     nil
   (singleton? a-seq) (first a-seq)
   :else              (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  ))


(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))
  ))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)            false
   (== (first a-seq) elem) true
   :else                     (sequence-contains? elem (rest a-seq))
  ))



(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else    '()
  ))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))
  ))


(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not (== (count a-seq) (count b-seq))) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false
  ))


(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else                               (cons
                                        (f (first seq-1) (first seq-2))
                                        (my-map f (rest seq-1) (rest seq-2)))
  ))


(defn power [n k]
  (cond
   (== k 0) 1
   :else (* n (power n (dec k)))
  ))

(defn fib [n]
  (cond
   (<= n 0) 0
   (== n 1) 1
   :else    (+ (fib (dec n)) (fib (dec (dec n))))
  ))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else                  (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  ))



(defn my-range [up-to]
  (cond
   (== up-to 0) '()
   :else         (cons (dec up-to) (my-range (dec up-to)))
  ))


(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else                 (cons (seq a-seq) (tails (rest a-seq)))
    ))



(defn inits [a-seq]
  (let [reversed (reverse a-seq)]
  (cond
   (empty? a-seq) '(())
   :else            (cons (seq a-seq) (inits (drop-last a-seq)))
  )))



(defn rotationHelper [t i counter]
  (cond
   (== counter 0)              '()
   (or (empty? t) (empty? i)) '(())
   :else                       (cons
                                (concat (first t) (first i))
                                (rotationHelper (rest t) (rest i) (dec counter))
  )))

(defn rotations [a-seq]
  (let [tailsFromA (reverse (tails a-seq))
        tailsFromB (inits a-seq)]
    (cond
     (empty? a-seq) '(())
     :else           (rotationHelper tailsFromA tailsFromB (count a-seq))
    )))



(defn my-frequencies-helper [freqs a-seq]
  (let [newFreq (concat freqs (frequencies a-seq))]
  (cond
   (empty? a-seq) freqs
   :else          (frequencies a-seq)
  )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (cond
   (singleton? a-map) (repeat (first(rest (first a-map))) (first (first a-map)))
   :else              (concat
                       (repeat (first(rest (first a-map))) (first (first a-map)))
                       (un-frequencies (rest a-map)))
  ))



(defn my-take [n coll]
  (cond
   (== n 0) '()
   (empty? coll) '()
   :else     (cons (first coll) (my-take (dec n) (rest coll)))
  ))


(defn my-drop [n coll]
  (cond
   (== n 0) coll
   :else    (my-drop (dec n) (rest coll))
  ))


(defn halve [a-seq]
  (let [middle (int(/ (count a-seq) 2))]
    (into [] (cons (my-take middle a-seq) (cons (my-drop middle a-seq) ())))
  ))


(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)                  b-seq
   (empty? b-seq)                  a-seq
   (> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   :else                           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))

  ))



(defn merge-sort [a-seq]
  (let [firstHalved (first (halve a-seq))
        secondHalved (second (halve a-seq))]
  (cond
   (or
    (== (count a-seq) 1)
    (== (count a-seq) 0))    (into () a-seq)
   :else                     (seq-merge (merge-sort firstHalved) (merge-sort secondHalved))
  )))



(defn split-into-monotonics [a-seq]
  (let [preFix (reverse (inits a-seq))]
    (cond
     (empty? a-seq) '()
     :else           ()
  )))



(defn permutations [a-set]
  ()
  )



(defn powerset [a-set]
  [:-])

