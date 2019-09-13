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
  (if (or (singleton? coll)
          (empty? coll))
    (first coll)
    (my-last (rest coll)))) 

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
      (if (pred? head)
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)(empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (and (= (first a-seq)(first b-seq))
        (seq= (rest a-seq)(rest b-seq))) true
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== k 0) 1
   (even? k) (let [p (power n (/ k 2))]
               (* p p))
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (< up-to 1) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

;; (defn rotations [a-seq]
;;   (let [r-fn (fn inner-rotate [coll]
;;                (let [r (concat (rest coll) (list (first coll)))]
;;                  (cond
;;                   (empty? coll) '(())
;;                   (= r a-seq) (list r)
;;                   :else (cons r (inner-rotate r)))))]
;;     (r-fn a-seq)))


(defn rotations-helper [n a-seq]
  (if (== n 1)
    (list (seq a-seq))
    (cons (seq  a-seq)
          (rotations-helper (dec n)
                            (concat (rest a-seq)(list (first a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [primo (first a-seq)
          new-freqs (assoc freqs primo (inc (get freqs primo 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll) '()
   (= n 0) '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (= n 0) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        first-half (my-take half a-seq)
        second-half (my-drop half a-seq)]
    (vector first-half second-half)))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) (list)
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [f-a (first a-seq)
               f-b (first b-seq)]
           (if (< f-a f-b)
             (cons f-a (seq-merge (rest a-seq) b-seq))
             (cons f-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) a-seq
      (let [[half-1 half-2] (halve a-seq)
            sorted-1 (merge-sort half-1)
            sorted-2 (merge-sort half-2)]
        (seq-merge sorted-1 sorted-2))))

(defn mono-inc?
  [coll]
  (apply <= coll))

(defn mono-dec?
  [coll]
  (apply >= coll))

(defn mono-helper
  [coll]
  (if (empty? coll)
    '()
    (let [[inc-first inc-rest] (split-with mono-inc? coll)
          ;; compute the next sequence of numbers--drop prefix
          next-inc (map #(drop (count (last inc-first)) %) inc-rest)
          [dec-first dec-rest] (split-with mono-dec? coll)
          next-dec (map #(drop (count (last dec-first)) %) dec-rest)]
      (if (> (count inc-first) (count dec-first))
        (cons (last inc-first) (mono-helper next-inc))
        (cons (last dec-first) (mono-helper next-dec))))))

(defn split-into-monotonics [a-seq]
  (if (< (count a-seq) 3)
    a-seq
    (mono-helper (rest (rest (reverse (inits a-seq)))))))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (mapcat (fn [elem]
           (map (fn [perm]
                  (cons elem perm))
                (permutations (remove #{elem} a-seq))))
         a-seq)))

(defn my-powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (apply clojure.set/union
           (map (fn [elem]
                  (let [p-set (my-powerset (disj a-set elem))]
                    (clojure.set/union
                     p-set
                     (set (map (fn [s]
                                 (clojure.set/union #{elem}
                                                    s)) p-set)))))
                (seq a-set)))))

(defn powerset [a-seq]
  (seq (my-powerset (set a-seq))))
