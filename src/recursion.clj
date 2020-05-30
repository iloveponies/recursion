(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))
  ))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))
  )
)

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll))
    )
  )
)

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
       (first a-seq)
       (max (first a-seq) (max-element (rest a-seq)))
    )
  )
)

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1
  )
)

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
       (first a-seq)
       (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    )
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (let [x (first a-seq)]
        (if (pred? x)
          (cons x (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))
      )
  ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                           (my-take-while pred? (rest a-seq)))
    :else (my-take-while pred? '())
  )
)

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq)
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (empty? b-seq) (not (empty? a-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))
  ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2))
      )
   )
)

(defn power [n k]
  (cond
    (= k 0) 1
    (= k 1) n
    :else (* n (power n (dec k)))
    )
)

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)) )
    )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times)
                                     what-to-repeat))
  )
)

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)) )
    )
  )

(defn tails [a-seq]
  (if (empty? a-seq) (cons '() ())
      (cons (sequence a-seq) (tails (rest a-seq)))
  ))

(defn inits [a-seq]
  (let [reversed (reverse a-seq)]
    (map reverse (tails reversed))
  ))

(defn rot-helper [len ct mseq]
  (if (= 0 ct) ()
     (cons (take len mseq) (rot-helper len (dec ct) (rest mseq)) ) )
  )

(defn rotations [a-seq]
  (if (empty? a-seq) (cons '() ())
    (let [rots (count a-seq)
          seqseq (concat a-seq a-seq)]
          (rot-helper rots rots seqseq)
          )
    )
  )

(defn my-frequencies-helper [freqs a-seq]
  (let [updater (fn [res-map entry]
                    (let [elem (get res-map entry)]
                      (if elem
                        (assoc res-map entry (inc elem))
                        (assoc res-map entry 1)
                      ))
        )]
        (if (empty? a-seq) freqs
          (my-frequencies-helper (updater freqs (first a-seq)) (rest a-seq))
        )
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) ()
      (let [key-val (first a-map)
            key (first key-val)
            val (last key-val)]
      (concat (repeat val key) (un-frequencies (rest a-map)))
      )
    )
    )

(defn my-take [n coll]
  (cond
    (= 0 n) ()
    (empty? coll) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))
  ))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (= n 0) coll
    :else (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [len (count a-seq)
        pre-len (int (/ len 2))]
    (conj (conj [] (my-take pre-len a-seq))
          (my-drop pre-len a-seq))
  ))

(defn seq-merge-helper [merged a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) merged
    (empty? a-seq) (concat merged b-seq)
    (empty? b-seq) (concat merged a-seq)
    (<= (first a-seq) (first b-seq)) (seq-merge-helper (conj merged (first a-seq)) (rest a-seq) b-seq)
    :else (seq-merge-helper (conj merged (first b-seq)) a-seq (rest b-seq))
    )
  )

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq)
  )

(defn merge-sort [a-seq]
  (if  (< (count a-seq) 2)
    a-seq
    (let [partitions (halve a-seq)]
      (seq-merge (merge-sort (get partitions 0)) (merge-sort (get partitions 1)))
    )
   )
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
