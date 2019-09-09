(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
    )
  )

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))
    )
  )

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))
      )
    )
  )

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
    )
  )

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1
    )
  )

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    )
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
      )
    )
  )

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq)
          false
        (= elem (first a-seq))
          true
        :else
          (sequence-contains? elem (rest a-seq))
        )
  )

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq)
          '()
        (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
        :else
          a-seq
        )
  )

(defn seq= [a-seq b-seq]
  (cond (not (= (count a-seq) (count b-seq)))
          false
        (and (empty? a-seq) (empty? b-seq))
          true
        (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))
        :else
          false
        )
  )

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2))
        '()
        :else
          (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
        )
  )

(defn power [n k]
  (cond (zero? k)
          1
        (zero? n)
          0
        :else
          (* n (power n (dec k)))
        )
  )

(defn fib [n]
  (cond (zero? n)
          0
        (= 1 n)
          1
        :else
          (+ (fib (dec n)) (fib (dec (dec n))))
        )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond (pos? how-many-times)
          (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
        :else
          ()
        )
  )

(defn my-range [up-to]
  (cond (zero? up-to)
          ()
        :else
          (cons (dec up-to) (my-range (dec up-to)))
        )
  )

(defn tails [a-seq]
  (cond (empty? a-seq)
          [()]
        :else
          (cons (seq a-seq) (tails (rest a-seq)))
        )
  )

(defn inits [a-seq]
  (cond (empty? a-seq)
          [()]
        :else
          (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))
        )
  )

(defn rotate-n [a-seq n-args]
  (cond (pos? n-args)
           (conj (rotate-n (concat (rest a-seq) [(first a-seq)]) (dec n-args))
                 (concat (rest a-seq) [(first a-seq)]))
        :else
          ()
        )
  )

(defn rotations [a-seq]
    (cond (empty? a-seq)
            (conj ()())
          :else
            (rotate-n a-seq (count a-seq))
    )
)

(defn my-frequencies-helper [freqs a-seq]
  (let [inc-add (fn [list elem]
                  (if (nil? (get list elem))
                    (assoc list elem 1)
                    (assoc list elem (inc (get list elem)))
                    )
                  )]
    (cond (empty? a-seq)
            freqs
          :else
            (my-frequencies-helper (inc-add freqs (first a-seq)) (rest a-seq))
          )
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  (let [destr-repeat (fn [elem]
                       (repeat (first (rest elem)) (first (seq elem)))
                       )]

   (if (not (empty? a-map))
     (concat (destr-repeat (first a-map)) (un-frequencies (rest a-map)))
     )
    )
  )

(defn my-take [n coll]
  (cond (> n (count coll))
          (seq coll)
        (< 0 n)
          (cons (first coll)(my-take (dec n) (rest coll)))
        )
  )

(defn my-drop [n coll]
   (cond (<= 1 n)
           (my-drop (dec n) (rest coll))
         :else
           coll
         )
  )

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))
        fh (or (my-take n a-seq) ())
        ]
    (cons fh [(my-drop n a-seq)])
    )
  )

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq)
          b-seq
        (empty? b-seq)
          a-seq
        (<= (first a-seq) (first b-seq))
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (> (first a-seq) (first b-seq))
          (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        )
  )

(defn merge-sort [a-seq]
  (cond (< 1 (count a-seq))
             (seq-merge (merge-sort(first (halve a-seq))) (merge-sort (second (halve a-seq))))
        (empty? a-seq)
             ()
        :else
             a-seq
           )

  )

(defn less-than [n]
  (fn [k] (< k n))
  )

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq)
          ()
        (pred? (first a-seq))
          (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else
          ()
        )
  )

(defn take-while-less [elem a-seq]
    (cond (empty? a-seq)
            nil
          (<= elem (first a-seq))
            (cons (first a-seq) (take-while-less (first a-seq) (rest a-seq)))
          )
  )

(defn take-while-greater [elem a-seq]
    (cond (empty? a-seq)
            nil
          (>= elem (first a-seq))
            (cons (first a-seq) (take-while-greater (first a-seq) (rest a-seq)))
          )
  )

(defn remove-n-elems [n a-seq]
  (cond (empty? a-seq)
          ()
        (zero? n)
          a-seq
        :else
          (remove-n-elems (dec n) (rest a-seq))
        )
  )

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq)
          ()
        (<= (first a-seq) (second a-seq))
          (cons (take-while-less (first a-seq) a-seq) (split-into-monotonics (remove-n-elems (count (take-while-less (first a-seq) a-seq)) a-seq)))
        (>= (first a-seq) (second a-seq))
          (cons (take-while-greater (first a-seq) a-seq) (split-into-monotonics (remove-n-elems (count (take-while-greater (first a-seq) a-seq)) a-seq)))
        )
  )

(defn permutations [a-set]
   (cond (empty? a-set)
           '(())
         (singleton? a-set)
           (list a-set)
         :else
           (for [head a-set
                  tail (permutations (disj (set a-set) head))]
              (do (cons head tail))
             )
         )
  )

(defn powerset [a-set]
  [:-])
