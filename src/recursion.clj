(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false
      )))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max (first a-seq)
         (if (= nil (max-element (rest a-seq)))
           0
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
    ))

(defn longest-sequence [a-seq]
   (if (empty? a-seq)
      nil
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (not (= (first a-seq) elem))
      (sequence-contains? elem (rest a-seq))
   :else
       true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
       ()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (not (pred? (first a-seq)))
      a-seq
   :else
       (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (if (= (count a-seq) (count b-seq))
    (if (or (empty? a-seq) (empty? b-seq))
    (if (= (first a-seq) (first b-seq))
      true
      false
    )
  (if (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
    false
  ))
    false
  )
)

(defn my-map [f seq-1 seq-2]
    (if (and (not (nil? (first seq-1))) (not (nil? (first seq-2))))
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
      ()
    )
)

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [ut (dec up-to)]
  (if (< up-to 1)
    ()
    (cons ut (my-range (dec up-to))))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (sequence a-seq) (tails (drop 1 a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (sequence a-seq) (inits (reverse (drop 1 (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (< 0 (count a-seq))
    (drop 1 (map concat (tails a-seq) (reverse (inits a-seq))))
    (map concat (tails a-seq) (reverse (inits a-seq)))
))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
    )
  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[k v]] (repeat v k) ) a-map)))

(defn my-take [n coll]
  (if (>= n (count coll))
    (sequence coll)
    (if (< n 1)
      ()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (>= n (count coll))
    ()
    (if (< n 1)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (if (empty? a-seq)
    ()
    [(reverse (my-drop (- (count a-seq) (int (/ (count a-seq) 2))) (reverse a-seq))) (my-drop (int (/ (count a-seq) 2)) a-seq)]))

(defn seq-merge-helper [index num b-seq]
  (if (>= index (count b-seq))
    index
    (if (and (< num (nth b-seq index)))
      index
      (seq-merge-helper (+ 1 index) num b-seq)
    ))
)

(defn first-part [n b-seq]
  (reverse (my-drop (- (count b-seq) (seq-merge-helper 0 n b-seq)) (reverse b-seq)))
  )

(defn last-part [n b-seq]
  (my-drop (seq-merge-helper 0 n b-seq) b-seq)
  )


(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (seq-merge (rest a-seq) (concat (first-part (first a-seq) b-seq) (cons (first a-seq) ()) (last-part (first a-seq) b-seq)))
  )
)

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [[x y] (halve a-seq)]
      (seq-merge (merge-sort x) (merge-sort y)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

