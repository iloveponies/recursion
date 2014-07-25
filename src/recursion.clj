(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
    true
    false))

(defn my-last [a-seq]
  ; Check for an empty seq.
  (if (empty? a-seq)
    (first a-seq)
    ; If not an empty seq, check if it is a singleton.
    (if (singleton? a-seq)
      (first a-seq)
      ; if not a singleton, recur with the rest of the seq.
      (my-last (rest a-seq)))))

(defn max-element [a-seq]
  (cond
   ; check for empty
   (empty? a-seq) nil
   ; reached the end of the seq
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [seq1-count (count seq-1)
        seq2-count (count seq-2)]
    (if (> seq1-count seq2-count)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  "This function returns the longest sequence given in a-seq. Looks like max-element!"
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    (rest a-seq)
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (= elem (first a-seq)) true
   (empty? (rest a-seq)) false
   :else (sequence-contains? elem (rest a-seq)))
  )

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) (rest a-seq)
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else `()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) (rest a-seq)
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) `()
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map + (rest seq-1) (rest seq-2)))))

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

