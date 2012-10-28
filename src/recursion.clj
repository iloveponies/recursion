(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; Exercise 2
;   The call to prduct begins by inspecting coll. If coll is empty, product immediately returns 1.
;   If coll is not empty, product takes its first element and multiplies it with the product of
;   the rest of the elements of coll. The value 1 is the base case of the algorithm, which
;   determines when the calculation stops. If we did not have a base case, the calculation would
;   continue infinitely.

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  ;empty collection?
  (if (empty? coll)
    nil
    ;singleton collection?
    (if (singleton? coll)
      (first coll)
      ;collection with > 1 items
      (my-last (rest coll)))))

(defn max-element [a-seq]
  ;empty sequence
  (if (empty? a-seq)
    nil
    ;singleton sequence
    (if (singleton? a-seq)
      (first a-seq)
      ;sequence with > 1 items
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [s1empty (empty? seq-1)
        s2empty (empty? seq-2)]
    ;if both sequences are empty
    (if (and s1empty s2empty)
      nil
      (if s1empty
        seq-1
        seq-2))))

(defn longest-sequence [a-seq]
  ;empty seq
  (if(empty? a-seq)
    nil
    ;singleton seq
    (if (singleton? a-seq)
      (first a-seq)
      ;two items seq
      (if (singleton? (rest a-seq))
        (seq-max (first a-seq) (second a-seq))
        ;multi-item seq
        (seq-max
          (seq-max (first a-seq) (second a-seq))
          (longest-sequence (rest a-seq)))))))

(defn my-last [coll]
  ;empty collection?
  (if (empty? coll)
    nil
    ;singleton collection?
    (if (singleton? coll)
      (first coll)
      ;collection with > 1 items
      (my-last (rest coll)))))

(defn max-element [a-seq]
  ;empty sequence
  (if (empty? a-seq)
    nil
    ;singleton sequence
    (if (singleton? a-seq)
      (first a-seq)
      ;sequence with > 1 items
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [s1empty (empty? seq-1)
        s2empty (empty? seq-2)]
    ;if both sequences are empty
    (if (and s1empty s2empty)
      nil
      (if s1empty
        seq-1
        seq-2))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
      (if (empty? (rest a-seq))
        (first a-seq)
        (seq-max [1 666 666 2 4] (first a-seq)))))

(defn my-filter [pred? a-seq]
  [:-])

(defn sequence-contains? [elem a-seq]
  :-)

(defn my-take-while [pred? a-seq]
  [:-])

(defn my-drop-while [pred? a-seq]
  [:-])

(defn seq= [a-seq b-seq]
  :-)

(defn my-map [f seq-1 seq-2]
  [:-])

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

