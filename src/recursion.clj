(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )

(defn singleton? [coll]
  (if (and (not (empty? (take 1 coll))) (empty? (rest coll)))
    true
    false)
  )

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll)))
  )

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (apply max (rest a-seq))))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2)
  )

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq))))
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))))
  )

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (== (first a-seq) elem)
      true
      (recur elem (rest a-seq))))
  )

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons
     (first a-seq)
     (my-take-while pred? (rest a-seq))))
  )

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (apply list a-seq)))
  )

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false)
  )

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
    ()
    :else
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (cond
    (= k 0) 1
    (= n 0) 0
    :else
    (* n (power n (- k 1))))
  )

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2))))
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat)))
  )

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1))))
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (if (empty? (rest a-seq))
      (list (apply list a-seq) ())
      (cons (apply list a-seq) (tails (rest a-seq)))
      ))
  )

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq))))
  )

                                        ;(defn rotations [a-seq]
                                        ;  (let [t (tails a-seq)
                                        ;        i (reverse (map reverse (tails (reverse a-seq))))]
                                        ;    (seq (set (map flatten (seq (zipmap t i))))))
                                        ;  )

(defn rotations [a-seq]
  (let [t (tails a-seq)
        i (reverse (map reverse (tails (reverse a-seq))))]
    (seq (set (map concat t i))))
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (if (nil? (freqs (first a-seq)))
       (assoc freqs (first a-seq) 1)
       (assoc freqs (first a-seq) (+ 1 (freqs (first a-seq)))))
     (rest a-seq)))
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat
     (repeat (second (first a-map)) (first (first a-map)))
     (un-frequencies (apply hash-map (flatten (rest a-map))))))
  )

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (list* (cons (first coll) (my-take (- n 1) (rest coll))))
    )
  )

(defn my-drop [n coll]
  (cond
    (empty? coll)
    ()
    (<= n 0)
    (list* coll)
    :else
    (my-drop (- n 1) (rest coll)))
  )

(defn halve [a-seq]
  (vector
   (my-take (int (/ (count a-seq) 2)) a-seq)
   (my-drop (int (/ (count a-seq) 2)) a-seq))
  )

(defn seq-merge-helper [m-seq a-seq b-seq]
  (cond
    (empty? a-seq)
    (concat m-seq b-seq)
    (empty? b-seq)
    (concat m-seq a-seq)
    :else
    (recur
     (conj m-seq
           (if (<= (first a-seq) (first b-seq)) (first a-seq) (first b-seq)))
     (if (<= (first a-seq) (first b-seq)) (rest a-seq) a-seq)
     (if (<= (first a-seq) (first b-seq)) b-seq (rest b-seq))
     ))
  )

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq)
  )

(defn merge-sort [a-seq]
  (cond (empty? a-seq)
        ()
        (== 1 (count a-seq))
        (list* a-seq)
        :else
        (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))
        ))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
  )

(defn split-into-monotonics-helper [m-seq a-seq]
  (if (empty? a-seq)
    m-seq

    (recur
     (conj (apply vector m-seq) (my-last (my-take-while monotonic? (rest (inits a-seq)))))
     (drop (count (my-last (my-take-while monotonic? (rest (inits a-seq))))) a-seq)))
  )

(defn split-into-monotonics [a-seq]
  (list* (split-into-monotonics-helper () a-seq))
  )

(defn split-seq [a-seq]
  (for [n (range (+ 1 (count a-seq)))]
    (split-at n a-seq)))

(defn my-ins [a-seq n]
  (map flatten
       (for [s (split-seq a-seq)]
         (list (first s) n (second s)))))

(defn permutations [a-seq]
  (cond (empty? a-seq)
        (list ())
        (= (count a-seq) 1)
        (list (apply list a-seq))
        (= (count a-seq) 2)
        (list (list (first a-seq) (second a-seq)) (list (second a-seq) (first a-seq)))
        :else
        (apply concat (for [b (permutations (rest a-seq))]
                        (let [a (first a-seq)] (my-ins b a))))
        )
  )

(defn powerset [a-set]
  (cond
    (empty? a-set)
    #{#{}}
    (= (count a-set) 1)
    #{(set a-set) #{}}
    (>= (count a-set) 2)
    (set (conj
          (list* (for [y (set a-set)
                       x (powerset (disj (set a-set) y))]
                   (#(conj % y) x)))
          #{}))
    :else
    #{#{}}
    )
  )

;; http://codereview.stackexchange.com/questions/12979/powerset-in-clojure
;(use '(clojure set))
;(defn powerset [ls]
;  (if (empty? ls) #{#{}}
;      (set (union (powerset (next ls))
;                (map #(conj % (first ls)) (powerset (next ls)))))))

