(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll)
                         (product (rest coll))))
  )

(defn singleton? [coll]
  (and (boolean (not-empty coll))
       (empty? (rest coll)))
  )

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll)))
  )

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq))))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2)
  )

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq))))
  )

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq)))
  )

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (if (= elem (first a-seq)) true
      (sequence-contains? elem (rest a-seq))))
  )

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      []))
  )

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) ()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq)
  )

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false)
  )

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k))))
  )

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (- n 2))))
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))
  )

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to))))
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq (tails (rest a-seq))))
  )

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
  )

(defn rotations [a-seq]
  (if (empty? a-seq) (seq [()])
    (map concat (rest (tails a-seq)) (rest (inits a-seq))))
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq) 0)))
                           (rest a-seq)))
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  ; duh there's a ready function for this L O L
  (flatten (map repeat (vals a-map) (keys a-map)))
  )

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n)) []
    (cons (first coll) (my-take (dec n) (rest coll))))
  )

(defn my-drop [n coll]
  (if (empty? coll) []
    (if (> n 0) (my-drop (dec n) (rest coll))
      (cons (first coll) (my-drop 0 (rest coll)))))
  )

(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq))
  )

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) []
        (empty? a-seq) (cons (first b-seq) (seq-merge (rest b-seq) []))
        (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) []))
        :else (if (< (first a-seq) (first b-seq))
                (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
                (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))
  )


(defn merge-sort [a-seq]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) a-seq
        :else (seq-merge (merge-sort (first (halve a-seq)))
                         (merge-sort (second (halve a-seq)))
                         )
        )
  )

; HARD EN-CORE ;;;;

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) []
    (let [i (inits a-seq)]
      (let [j
            (take-while (fn [x] (or (= x (merge-sort x)) (= x (reverse (merge-sort x))))) i)]
      (cons (last j) (split-into-monotonics (drop (dec (count j)) a-seq))))))
  )

(defn permutations [a-set]
  (if (empty? a-set) (list '())
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (cons head tail)))
  )

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [head (first a-set)
          sset (powerset (next a-set))]
      (clojure.set/union sset (map #(conj % head) sset))))
  )


; here's a face for you )))))))))))))))))))))))
