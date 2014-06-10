(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false
      (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (let [[a b & r] a-seq]
                (max-element (cons (max a b) r)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (let [[seq-1 seq-2 & r] a-seq]
               (longest-sequence (cons (if (> (count seq-1) (count seq-2)) seq-1 seq-2) r)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f) (cons f (my-filter pred? r))
                    (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
   (if (= elem (first a-seq))
     true
     (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (let [a-empty (empty? a-seq)
        b-empty (empty? b-seq)]
   (cond (and a-empty b-empty) true
         (or  a-empty b-empty) false
         (not (= (first a-seq) (first b-seq))) false
         :else (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (<= n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (loop [s '()
         n 0]
    (if (>= n up-to) s
      (recur (cons n s) (inc n)))))

(defn tails [a-seq]
  (if (empty? a-seq) [a-seq]
   (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
 (->>
  (loop [accum  ['()]
         s a-seq]
    (if (empty? s) accum
      (recur (cons (conj (first accum) (first s))
                   accum)
             (rest s))))
  (map reverse)))

(defn rotations [a-seq]
  (loop [accum  [a-seq]
         n (count (rest a-seq))]
    (if (zero? n) accum
      (recur (cons (cons (last (first accum)) (butlast (first accum)))
                   accum)
             (dec n)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          prev-ct (or (get freqs item) 0)
          new-freqs (assoc freqs item (inc prev-ct))
          new-seq (rest a-seq)]
      (my-frequencies-helper new-freqs new-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (->>  a-map
  (map #(my-repeat (second %) (first %)))
  (apply concat)))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll)) (empty coll)
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (>= 0 n) (empty? coll)) coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [ct (count a-seq)
        half-ct (Math/floor (/ ct 2))]
    [(my-take half-ct a-seq)
     (my-drop half-ct a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [[a & next-a] a-seq
              [b & next-b] b-seq]
          (if (< a b) (cons a (seq-merge next-a b-seq))
            (cons b (seq-merge a-seq next-b))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (== 1 (count a-seq))) a-seq
      (apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
  (let [helper
          (fn [accum monotonic the-seq]
            (if (empty? the-seq) (cons monotonic accum)
              (let [[f & r] the-seq]
                (if (or (empty? monotonic)
                        (empty? (rest monotonic))
                        (= (pos? (- f (first monotonic)))
                           (pos? (- (first monotonic) (second monotonic)))))
                  (recur accum (cons f monotonic) r)
                  (recur (cons monotonic accum) [f] r)))))]
    (->> a-seq
         (helper [] [])
         (map reverse)
         reverse)))

(defn permutations [a-set]
  (let [permute
          (fn permute [so-far remainder]
            (if (empty? remainder) [so-far]
              (apply concat (map (fn [n] (permute (cons n so-far) (remove #(= % n) remainder))) remainder))))]
    (permute [] a-set)))

(defn powerset [a-set]
   #{#{}})

;; Exercise 29
;; 3 points
;; Given a set, return the powerset of that set.
;; (powerset #{})      ;=> #{#{}}
;; (powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
