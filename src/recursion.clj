(ns recursion)

(defn product [coll]
  (let [prod-helper
        (fn [acc coll]
          (if  (empty? coll)
            acc
            (recur (* acc (first coll)) (rest coll))))]
       (prod-helper 1 coll)
  )
)



(defn singleton? [coll]
  (if (empty? coll)
      false
      (empty? (rest coll)))
)

(defn my-last [coll]
  (cond
     (empty? coll) nil
     (singleton? coll) (first coll)
     :else (recur (rest coll))))


(defn max-element [coll]
  (let  [helper (fn [best coll]
                  (if (empty? coll) best
                      (recur (max best (first coll)) (rest coll))))]
  (if (empty? coll) nil
      (helper (first coll) (rest coll)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1) c2 (count seq-2)]
    (if (> c1 c2) seq-1 seq-2)
  )
)

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
      (reduce seq-max a-seq))
)

(defn my-filter [pred? a-seq]
  (let [helper
        (fn [acc pred? a-seq]
            (let [head (first a-seq)]
                 (cond (empty? a-seq) acc
                        (pred? head) (recur (conj acc head) pred? (rest a-seq))
                        :else  (recur acc pred? (rest a-seq)))))]
    (helper [] pred? a-seq)))


(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq) false
     (== elem (first a-seq)) true
     :else (recur elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (let [ head (first a-seq)]
    (cond
       (empty? a-seq) a-seq
       (pred? head)   (cons head (my-take-while pred? (rest a-seq)))
       :else          []
     )
))


(defn my-drop-while [pred? a-seq]
    (cond
       (empty? a-seq) a-seq
       (pred? (first a-seq))   (my-drop-while pred? (rest a-seq))
        :else a-seq
     )
)


(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (not (= (count a-seq) (count b-seq))) false
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false
  )
)


(defn my-map [f seq-1 seq-2]
  (let [helper (fn [acc f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2)) acc
        (let [ result (f (first seq-1) (first seq-2))
               next-acc (conj acc result)]
           (recur next-acc f (rest seq-1) (rest seq-2))
        )
    )
  )]
  (helper [] f seq-1 seq-2)
))


(defn power [n k]
  (let [helper
        (fn [acc n k]
          (if (zero? k)
            acc
            (recur (* n acc) n (dec k))))]
  (helper 1 n k)))


(defn fib [n]
 (let [helper
       (fn [f-0 f-1 n]
         (if
          (zero? n) f-0
          (recur f-1 (+ f-0 f-1) (dec n))))]
       (helper 0 1 n)
  )
)


(defn my-repeat [how-many-times what-to-repeat]
  (let [helper (fn [acc n rep]
                (if (<= n 0) acc
                    (recur (conj acc rep) (dec n) rep)))]

    (helper [] how-many-times what-to-repeat)
  )
)

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (dec up-to) (my-range (dec up-to))))
)

(defn tails [a-seq]
   (let [helper (fn [acc a-seq]
                  (if (empty? a-seq) (seq (conj acc ()))
                      (recur (conj acc (seq a-seq)) (rest a-seq))))]
     (helper [] a-seq)
))


(defn inits [a-seq]
   (let [helper (fn [acc a-seq]
                  (if (empty? a-seq) (seq (conj acc ()))
                      (recur (conj acc (seq (reverse a-seq))) (rest a-seq))))]
     (helper [] (reverse a-seq))
))


(defn rotations [a-seq]
  (let [helper (fn [acc n a-seq]
                 (let
                   [ rotate (fn [x] (conj (vec (rest x)) (first x)))
                     rotated (rotate a-seq)]
                     (if (zero? n)  acc
                         (recur (conj acc rotated) (dec n) rotated))
                   ))]
    (if (empty? a-seq) [[]]
    (helper [] (count a-seq) a-seq))
))

(defn my-frequencies [x]
  (let [helper (fn [freqs x]
                   (if (empty? x) freqs
                       (let [head     (first x)
                             freq     (if (contains? freqs head) (get freqs head) 0)
                             new-freq (assoc freqs head (inc freq))]
                        (recur new-freq (rest x)))))]
    (helper {} x)
))

(defn un-frequencies [a-map]
    (let [helper (fn [acc maps]
                   (if  (empty? maps)
                        acc
                        (let [ [rep n]   (first maps)
                               repeats   (repeat n rep)
                               next-acc  (concat acc repeats) ]
                        (recur next-acc (rest maps)))))]
      (helper [] a-map)
))

(defn my-take [n coll]
    (if
     (or (empty? coll) (zero? n)) []
     (cons (first coll) (my-take (dec n) (rest coll))))
)

(defn my-drop [n coll]
    (if
     (or (empty? coll) (zero? n)) coll
     (my-drop (dec n) (rest coll)))
)


(defn halve [a-seq]
  (let [length (count a-seq)
        half (int (/ length 2))
        left (my-take half a-seq)
        right (my-drop half a-seq)
        ]
    [left right]
   ))


(defn seq-merge [a-seq b-seq]
  (let [helper (fn [acc a-seq b-seq]
    (cond
     (empty? a-seq) (concat acc b-seq)
     (empty? b-seq) (concat acc a-seq)
     :else (let [head-a    (first a-seq)
                 head-b    (first b-seq)
                 a-first?  (< head-a head-b)
                 next-acc  (if a-first?
                             (conj acc head-a)
                             (conj acc head-b))
                 rest-a    (if a-first? (rest a-seq) a-seq)
                 rest-b    (if a-first? b-seq (rest b-seq))]

             (recur next-acc rest-a rest-b))))]
   (helper [] a-seq b-seq)
))


(defn merge-sort [a-seq]
   (cond
    (empty? a-seq) []
    (singleton? a-seq) a-seq
    :else (let [ [left right] (halve a-seq)]
       (seq-merge
        (merge-sort left)
        (merge-sort right))))
)

(defn monotonic? [a-seq]
  (or (apply <=  a-seq)
      (apply >=  a-seq)))


(defn split-into-monotonics [a-seq]
    (let [helper
      (fn [acc cur a-seq]
         (if
            (empty? a-seq) (conj acc cur)
            (let [ cur-candidate (conj cur (first a-seq) )
                   keep?         (monotonic? cur-candidate )
                   next-cur      (if keep? cur-candidate [(first a-seq)])
                   next-acc      (if keep? acc (conj acc cur)) ]
            (recur next-acc next-cur (rest a-seq)))))]

    (helper [] [(first a-seq)] (rest a-seq))
))



(defn powerset [a-set]
  (if
     (empty? a-set) [a-set]
     (let [pow-rest (powerset (rest a-set))]
     (concat (map (fn [x] (conj x (first a-set))) pow-rest) pow-rest)))
)


(defn spread [a-set]
  (map #(disj (set a-set) %) a-set))

(defn inject [el perms]
  (map #(cons el %) perms))

(defn permutations [a-set]
    (if (empty? a-set)  [[]]
        (let [spreads  (spread a-set)
              perms    (map permutations spreads) ]
         (reduce  concat (map inject a-set perms)))))

