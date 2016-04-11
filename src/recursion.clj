(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (if (singleton? a-seq)
        a-seq
        (cons
          (first a-seq)
          (my-filter pred? (rest a-seq))))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    '()
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    '()
    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    :else a-seq
    ))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    )
  )

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (cond
    (== k 0) 1
    :else (* n (power n (dec k))))
  )

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) '()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

(defn my-range [up-to]
  (cond
    (<= up-to 0) '()
    :else (cons (dec up-to) (my-range (dec up-to)))
    ))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (cons a-seq '())
    :else (cons a-seq (tails (rest a-seq)))
    ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq)))
  )

(defn my-rotations [a-seq n]
  (let [first (first a-seq)
        rotated (concat (rest a-seq) (cons first '()))]
    (cond
      (> n 0) (cons a-seq (my-rotations rotated (dec n)))
      :else '()
      )
    ))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) (cons '() '())
    :else (my-rotations a-seq (count a-seq)))
  )


(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq)
    freqs
    :else
    (let [
          f (first a-seq)
          b (boolean (freqs f))
          v (if b
              (+ (freqs f) 1)
              1)
          ]
      (my-frequencies-helper (assoc freqs f v) (rest a-seq))
      )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) '()
    :else
    (let [key (first (keys a-map))]
      (concat
        (my-repeat (a-map key) key)
        (un-frequencies (dissoc a-map key))
        ))))

(defn my-take [n coll]
  (cond
    (== n 0) '()
    (empty? coll) '()
    :else
    (cons
      (first coll)
      (my-take (dec n) (rest coll)))
    )
  )

(defn my-drop [n coll]
  (cond
    (== n 0) coll
    (empty? coll) coll
    :else (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [size1 (int (/ (count a-seq) 2))]
    (cons
      (my-take size1 a-seq)
      (cons (my-drop size1 a-seq) '())
      )
    ))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (let [
          f (first a-seq)
          test? (fn [x] (<= x f))
          pref (my-take-while test? b-seq)
          suff (my-drop-while test? b-seq)
      ]
      (concat
        pref
        (cons f '())
        (seq-merge (rest a-seq) suff))
      )))

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    (== (count a-seq) 2)
      (if (< (first a-seq) (second a-seq))
        (cons (first a-seq) (cons (second a-seq) '()))
        (cons (second a-seq) (cons (first a-seq) '())))
    :else
        (seq-merge
          (merge-sort (first (halve a-seq)))
          (merge-sort (second (halve a-seq))))
    ))

(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

