(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
    ))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))
    ))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))
    ))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
    ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
    ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (conj (my-filter pred? (rest a-seq)) (first a-seq))
      (my-filter pred? (rest a-seq))
      )
    ))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))
    ))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (conj (my-take-while pred? (rest a-seq)) (first a-seq))
      '())
    ))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq
      )
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
        false
    (not (= (first a-seq) (first b-seq)))
        false
    :else (seq= (rest a-seq) (rest b-seq))
    ))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else (conj (my-map f (rest seq-1) (rest seq-2)) (f (first seq-1) (first seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)
    ))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (conj (my-range (dec up-to)) (dec up-to))
    ))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (conj '() '())
    :else
      (conj (tails (rest a-seq)) a-seq)
    ))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
  )

(defn rotations [a-seq]
  (cond
    (empty? a-seq) (conj '() '())
    :else (let [rot-seq (range (count a-seq))]
            (map (fn [r]
                   (concat
                     (drop r a-seq)
                     (take r a-seq)))
                 rot-seq)
            )
    )
  )

(defn my-frequencies-helper [freqs a-seq]
  (cond (empty? a-seq) freqs
        :else (let [
                    el (first a-seq)
                    restseq (rest a-seq)
                    next-val (if (contains? freqs el)
                               (inc(get freqs el))
                               1)
                    ]
                (my-frequencies-helper (assoc freqs el next-val) restseq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat '() (map (fn [[key val]] (repeat val key)) a-map))
  )

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (conj (my-take (dec n) (rest coll)) (first coll))
    ))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [
        sep (int (/ (count a-seq) 2))
        first-half (take sep a-seq)
        second-half (drop sep a-seq)
        ]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (let [
          a-el (first a-seq)
          b-el (first b-seq)
          a-rest (rest a-seq)
          b-rest (rest b-seq)
          smaller-el  (min a-el b-el)
          new-seq-a (if (< a-el b-el) a-rest a-seq)
          new-seq-b (if (< a-el b-el) b-seq b-rest)
          ]
        (conj (seq-merge new-seq-a new-seq-b) smaller-el)
      )
    ))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [
          [half1 half2] (halve a-seq)
          sorted-half1 (merge-sort half1)
          sorted-half2 (merge-sort half2)
          ]
      (seq-merge sorted-half1 sorted-half2))
    ))

(defn split-into-monotonics [a-seq]
  (cond
    (zero? (count a-seq)) '()
    :else (let [
                starters (inits a-seq)
                monotonic-starters (take-while (fn [s] (or (<= (count s) 1) (apply <= s) (apply >= s))) starters)
                longest-part  (last monotonic-starters)
                reduced-seq (drop (count longest-part) a-seq)
              ]
            (conj (split-into-monotonics reduced-seq) longest-part))
  ))

(defn permutations [a-set]
  (if (<= (count a-set) 1)
    (seq a-set)
    (let [
          a-seq (seq a-set)
          el (first a-seq)
          perm (permutations (set (rest a-seq)))
          pos (range (count a-seq))
          comb-perm (map (fn [po]
                           (map (fn [pe]
                                  (concat
                                    (take po pe)
                                    (conj (drop po pe) el)
                                    )
                                  )
                                perm
                                )
                           )
                         pos)
          flattened-combs (apply concat comb-perm)
          ]
      flattened-combs
      ))
  )

(defn powerset [a-set]
  (cond (empty? a-set) #{#{}}
        :else (let [
                    el (first a-set)
                    rest-set (set(rest a-set))
                    result-before (powerset rest-set)
                    mapped-result (map (fn [x] #{x (conj x el)}) result-before)
                    result (apply concat mapped-result)
                    ]
                (set result))
    ))

