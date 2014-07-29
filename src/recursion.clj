(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
  (* (first coll)
     (product (rest coll)))
  ))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (cond
   (or (nil? coll) (empty? coll)) nil
   (singleton? coll) (first coll)
    :else (my-last (rest coll))))


(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))



(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)) )))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [the-first (first a-seq) the-rest (rest a-seq)]
      (if (pred? the-first) (cons the-first (my-filter pred? the-rest)) (my-filter pred? the-rest)))))


(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '() (let [the-first (first a-seq)]
                             (if (pred? the-first) (cons the-first  (my-take-while pred? (rest a-seq))) ()))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))


(defn seq= [a-seq b-seq]
  (cond (not= (count a-seq) (count b-seq)) false
        (and (empty? a-seq) (empty? b-seq)) true
        :else
        (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        )))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))) )

(defn fib [n]
  (cond (zero? n) 0
   (= 1 n) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (<  how-many-times 1) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) () (let [n-1 (dec up-to)] (cons n-1 (my-range n-1)))))

(defn tails [a-seq]

  (if (empty? a-seq) [()] (let [the-tail (rest a-seq )] (cons   (seq a-seq) (tails the-tail)))))


(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate1 [n a-seq]
  (if (not= 0 n)  (cons  (seq a-seq) (rotate1 (dec n) (concat (vec (rest a-seq)) (vector (first a-seq))))))
  )

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (rotate1 (count a-seq) a-seq)))


(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])(defn my-frequencies-helper [freqs a-seq]

   (if (empty? a-seq ) freqs
    (let [my-key (first a-seq)
           my-val (get freqs my-key)]
                                   (my-frequencies-helper (assoc freqs my-key (if (nil? my-val) 1 (inc my-val ) )) (rest a-seq) ))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))



(defn un-frequencies [a-map]
  (if (empty? a-map) ()
              (let [
                    [key val] (first a-map)] (concat (repeat val key) (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  (if (and (pos? n ) (pos? (count coll))) (cons (first coll)
    (my-take (dec n) (rest coll))) '()))

(defn my-drop [n coll]
  (if (and (pos? n) (pos? (count coll))) (my-drop (dec n) (rest coll)) coll))


(defn halve [a-seq]
  (let [halfway (int (/ (count a-seq) 2))] (vector (my-take halfway a-seq)  (my-drop halfway a-seq))))

(defn seq-merge-helper [element b-seq] ()
  (cond (empty? b-seq)  [element]
        (> (count b-seq ) 0)
           (let [start-b (first b-seq)]
                        (if (< element start-b)
                          (cons element (cons start-b (rest b-seq)))
                          (cons start-b (seq-merge-helper element (rest b-seq)))))
        :else [(first b-seq) element]))



(defn seq-merge [a-seq b-seq]
   (if (empty? a-seq) b-seq
     (let [first-elem (first a-seq)] (seq-merge   (rest a-seq)
                                                 (seq-merge-helper first-elem b-seq) ))))


(defn merge-sort [a-seq]
  (let [size (count a-seq)]
    (cond (<= size 1) a-seq
          (= size 2) (if (<= (first a-seq)
                            (last a-seq)) a-seq
                             (reverse a-seq))
          :else (apply seq-merge (map merge-sort (halve a-seq)))

          ;:else  (let [[first-halve second-half] (halve (vec a-seq))] (seq-merge (merge-sort first-halve) (merge-sort second-half)))
          )))
(defn monotonic? [a-seq]
  (and  (> (count a-seq) 1 ) (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

