(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? (rest coll)) (if (empty? coll) false true) false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
       (my-last (rest coll))))


(defn max-element [a-seq]
(last (sort a-seq)))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (last (sort a-seq)))

(defn my-filter [pred a-seq]
(filter pred a-seq))


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
  (if (= elem (first a-seq)) true (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq))) ())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) ()
    (if (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq)) a-seq)))


(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq)) true
  (if (or (empty? a-seq) (empty? b-seq))  false
  (if (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)) false))))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (= k 0) 1
         (* n (power n (dec k)))))

(def fib
   (fn [n]
       (if (< n 1) 0
       (if (= n 1) 1
       (+ (fib (dec (dec n))) (fib (dec n)))))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)) '()))


(defn my-range [up-to]
  (if (< up-to 1) ()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
 (if (empty? a-seq) '(())
   (cons a-seq (tails (rest a-seq)))))

  (defn inits [a-seq]
  (reverse (if (empty? a-seq) '(())
   (cons a-seq (inits (butlast a-seq))))))



(defn rotations [a-seq]
   (defn my-repeat-rotatio [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat-rotatio (dec how-many-times)
(concat (rest what-to-repeat)[(first what-to-repeat)]))) '()))

 (if (empty? a-seq) '(()) (my-repeat-rotatio (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
(frequencies a-seq))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (defn un-freq-helper [unfreq a-map]
  (if (empty? a-map)
    unfreq
    (un-freq-helper (concat unfreq (repeat (val (first a-map)) (key (first a-map)))) (rest a-map))))
  (un-freq-helper () a-map))


(defn my-take [n coll]
  (if (and (> n 0) (not-empty coll))
   (cons (first coll) (my-take (dec n) (rest coll))) '()))


(defn my-drop [n coll]
    (if (and (> n 0) (not-empty coll))
      (my-drop (dec n) (rest coll))coll) )

(defn halve [a-seq]
  (concat [(my-take (int (/ (count a-seq) 2))a-seq)]
        [(my-drop (int (/ (count a-seq) 2))a-seq)]))


(defn seq-merge [a-seq b-seq]

(defn seq-merge-helper [r-seq a-seq b-seq]
  (cond
    (zero? (count a-seq))
      (concat r-seq b-seq)
    (zero? (count b-seq))
      (concat r-seq a-seq)
    (> (first a-seq) (first b-seq))
      (seq-merge-helper (conj r-seq (first b-seq)) a-seq (rest b-seq))
    (< (first a-seq) (first b-seq))
      (seq-merge-helper (conj r-seq (first a-seq)) (rest a-seq) b-seq)
    (= (first a-seq) (first b-seq))
      (seq-merge-helper (conj r-seq (first a-seq) (first b-seq)) (rest a-seq) (rest b-seq))))
;kesken

  (seq-merge-helper [] a-seq b-seq))


(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[b-seq c-seq] (halve a-seq)]
      (seq-merge (merge-sort b-seq)
                 (merge-sort c-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic? (fn [a-seq] (or (apply <= a-seq)
                                   (apply >= a-seq)))
          split-help (fn [pre suf]
                       (if (monotonic? pre)
                         (list pre suf)
                         (recur (butlast pre)
                                (cons (last pre)
                                      suf))))
          [pre suf] (split-help a-seq '())]
      (cons pre (split-into-monotonics suf)))))


(defn permutations [a-set]
  (if (empty? a-set)
  [a-set]
   (mapcat (fn [n] (map (fn [p] (concat [n] p))
              (permutations (filter (fn [e] (not (= e n))) a-set)))) a-set)))


(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [ps (powerset (rest a-set))]
    (concat ps
            (map #(conj % (first a-set)) ps)))))

