(ns recursion)
(require '[clojure.set :refer [union difference]])
(defn reload []
  (use 'recursion :reload))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element
  ([a-seq]
    (if (empty? a-seq)
      nil
      (max-element (first a-seq) (rest a-seq))))
  ([val a-seq]
    (if (empty? a-seq)
      val
      (max-element (max val (first a-seq)) (rest a-seq))
      )))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence
  ([a-seq]
    (if (empty? a-seq)
      nil
      (longest-sequence (first a-seq) (rest a-seq))))
  ([val a-seq]
    (if (empty? a-seq)
      val
      (longest-sequence (seq-max val (first a-seq)) (rest a-seq))
      )))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))
    ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    )
  )

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    )
  )

(defn power [n k]
  (if (zero? k)
    1
    (*
      n (power n (dec k)))
    ))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))
    )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () nil)
    (cons (seq a-seq) (tails (rest a-seq)))
    ))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () nil)
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))
    ))

(defn rotations
  ([a-seq]
    (if (empty? a-seq)
      (list ())
      (rotations a-seq 0)))
  ([a-seq c]
    (if (= (count a-seq) c)
      ()
      (let [e (cons (first (reverse a-seq)) (reverse (rest (reverse a-seq))))]
        (cons e (rotations e (inc c))))
      )
    ))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [val (first a-seq)
          new-freqs (if (contains? freqs val)
                      (assoc freqs val (inc (get freqs val
                                                 )))
                      (assoc freqs val 1)
                      )]
      (my-frequencies-helper new-freqs (rest a-seq)))
    ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[key count]] (repeat count key)) a-map)))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))
    ))

(defn my-drop [n coll]
  (cond
    (zero? n) coll
    (empty? coll) ()
    :else (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [size (count a-seq)
        div (int (/ size 2))
        fix (if (even? size) 0 1)
        h1 (- (- size fix) div)
        ]
    (list (my-take h1 a-seq) (seq (my-drop h1 a-seq)))
    )
  )

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq))))
            )
    )
  )

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq)) a-seq
                          (let [[a b] (halve a-seq)]
                            (seq-merge (merge-sort a) (merge-sort b))
                            )
                          ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [in (rest (reverse (inits a-seq)))
          mono (first (reverse (take-while monotonic? in)))
          l (count mono)]
      (if (empty? mono)
        (cons (first a-seq) (split-into-monotonics (rest a-seq)))
        (cons mono (split-into-monotonics (drop l a-seq)))
        )
      )
    )
  )

(defn permutations-helper [a-set a-list result]
  "This needs rework, should work with sequences and not only sets"
  (if (empty? a-set)
    (cons a-list result)
    (for [x a-set
          solution (permutations-helper (difference a-set #{x}) (cons x a-list) result)]
      solution
      )
    )
  )

(defn permutations [a-set]
  "
  (permutations #{})
  ;=> (())
  (permutations #{1 5 3})
  ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))
  "
  (permutations-helper a-set () ()))

(defn powerset-helper [a-set acc-set]
  "Expects acc-set to have empty set already."
  (if (empty? a-set)
    acc-set
    (powerset-helper (rest a-set) (union acc-set (map #(conj % (first a-set)) acc-set)))
    )
  )

(defn powerset [a-set]
  "(powerset #{})      ;=> #{#{}}
   (powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
  "
  (powerset-helper a-set #{#{}}))
