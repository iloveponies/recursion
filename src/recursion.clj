(ns recursion)

(defn product [coll]
    (if (empty? coll)
        1
        (* (first coll)
           (product (rest coll))
        )
    )
)

(defn singleton? [coll]
    (if (empty? (rest coll) )
        (if (empty? coll)
            false
            true
        )
        false
    )
)

(defn my-last [coll]
    (if (empty? (rest coll) )
        (first coll)
        (my-last (rest coll))
    )
)

(defn max-element [a-seq]
    (if (empty? a-seq)
        nil
        (let [b (max-element (rest a-seq))]
            (max (first a-seq) (if (nil? b) (first a-seq) b))
        )
    )
)


(defn sum [coll]
  (if (empty? coll)
    0
    (+ (first coll)
       (sum (rest coll))))
)

(defn seq-max [seq-1 seq-2]
    (if (> (count seq-1) (count seq-2))
        seq-1
        (if (= (count seq-1) (count seq-2))
            (if (>= (sum seq-1) (sum seq-2))
                seq-1
                seq-2
            )
            seq-2
        )
    )
)

(defn longest-sequence [a-seq]
    (if (empty? (rest a-seq))
        (first a-seq)
        (if (empty? (rest (rest a-seq)) )
            (seq-max (first a-seq) (first (rest a-seq)))
            (longest-sequence (seq (conj (rest (rest a-seq)) (seq-max (first a-seq) (first (rest a-seq))) ) ) )
        )
    )
    
)

(defn my-filter [pred? a-seq]
    (if (empty? a-seq)
    a-seq
    (let [b (first a-seq)]
      (if (pred? b)
        (concat [b] (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))
      )
    )
    )
)

(defn sequence-contains? [elem a-seq]
    (if (empty? a-seq)
        false
        (if (= elem (first a-seq))
            true
            (sequence-contains? elem (rest a-seq))
        )
    )
)

(defn my-take-while [pred? a-seq]
    (if (empty? a-seq)
        a-seq
        (if (pred? (first a-seq))
            (concat [(first a-seq)] (my-take-while pred? (rest a-seq)))
            []
        )
    )
)

(defn my-drop-while [pred? a-seq]
    (if (empty? a-seq)
        a-seq
        (if (pred? (first a-seq))
            (my-drop-while pred? (rest a-seq))
            a-seq
        )
    )
)

(defn seq= [a-seq b-seq]
    (if (empty? a-seq)
        (if (empty? b-seq)
            true
            false
        )
        (if (empty? b-seq)
            false
            (if (not (= (first a-seq) (first b-seq)))
                false
                (seq= (rest a-seq) (rest b-seq))
            )
        )
    )
)

(defn my-map [f seq-1 seq-2]
  (if (empty? seq-1)
        []
        (if (empty? seq-2)
            []
            (concat [(f (first seq-1) (first seq-2))] (my-map f (rest seq-1) (rest seq-2)))
        )
    )
)

(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

