(ns recursion)

(defn product [coll]
(if (empty? coll)
  1
  (* (first coll) (product (rest coll)))
  )
  )

(defn singleton1? [coll]
   (== (count (apply list coll)) 1)
  )

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (or (empty? (rest coll)) (nil? (rest coll)))
      true
      false
      )
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

(defn max-element1 [a-seq]
(if (empty? a-seq)
  nil
  (apply max a-seq)
  )
  )


(defn max-element [a-seq]
(if (empty? a-seq)
  nil
(max (first a-seq) (if (nil?(max-element (rest a-seq)))
                     0
                     (max-element (rest a-seq))
                     )
                     ))
  )


(defn seq-max [seq-1 seq-2]

  (if (== (count (apply list seq-1)) (count(apply list seq-2)))
    (if (> (max-element seq-1) (max-element seq-2))
      seq-1
      seq-2
      )
  (if (> (count (apply list seq-1)) (count(apply list seq-2)))
    seq-1
    seq-2
    )
    )

  )

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence  (rest a-seq)))
    )

  )

(defn my-filter [pred? a-seq]
 (if (empty? a-seq)
   a-seq
   (if (pred? (first a-seq))
   (cons  (first a-seq)
   (my-filter pred? (rest a-seq)))
     (my-filter pred? (rest a-seq))
)))

(defn sequence-contains? [elem a-seq]
 (cond
  (empty? a-seq)
  false

  (= (first a-seq) elem)
  true

  :else
  (sequence-contains? elem (rest a-seq)))

  )


(defn my-take-while [pred? a-seq]
(cond
(empty? a-seq)
'()
(pred? (first a-seq))
(cons (first a-seq) (my-take-while pred? (rest a-seq)))
:else
'()))


(defn my-drop-while [pred? a-seq]
(cond
(empty? a-seq)
'()
(pred? (first a-seq))
(my-drop-while pred? (rest a-seq))
:else
a-seq))


(defn seq= [a-seq b-seq]
(cond
(and (empty? a-seq) (empty? b-seq)) true
(or
(or (empty? a-seq) (empty? b-seq))
(not (= (first a-seq) (first b-seq)))) false
:else (seq= (rest a-seq) (rest b-seq)))
  )


(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2))
   '()
   :else
    (cons (f (first seq-1) (first seq-2))(my-map f (rest seq-1) (rest seq-2)))
   )
  )

(defn power [n k]

  (if (= k 0)
    1
    (* n (power n (dec k))))

  )


(defn fib [n]
(if (< n 1)
  0
  (if (< n 3)
    1
    (+ (fib(dec (dec n))) (fib(dec n)))
    )
  )
  )


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat  (my-repeat (dec how-many-times) what-to-repeat))
    )
  )

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
    )
)

(defn tails [a-seq]
(if (empty? a-seq)
'(())
(cons (reverse (into () a-seq)) (tails (rest a-seq)))))


(defn inits [a-seq]
(map reverse (tails (reverse a-seq))))


(defn rotations-helper [head-list tail-list]
(cond
(empty? tail-list) []
:else (cons (concat tail-list head-list) (rotations-helper (concat head-list [(first tail-list)]) (rest tail-list)))))

(defn rotations [a-seq]
(cond
(empty? a-seq) [[]]
:else (rotations-helper [] a-seq)))



(defn my-frequencies-helper [freqs a-seq]
(if (empty? a-seq)
freqs
(let [first-item (first a-seq)
curre-value (get freqs first-item)
new-fags (if (nil? curre-value)
(assoc freqs first-item 1)
(assoc freqs first-item (inc curre-value)))]
(my-frequencies-helper new-fags (rest a-seq)))))

(defn my-frequencies [a-seq]
(my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
(mapcat #(repeat (second %) (first %)) (seq a-map)))

(defn my-take [n coll]
(cond
(or (zero? n) (empty? coll)) []
:else (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
(cond
(or (zero? n) (empty? coll)) coll
:else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
(let [h (int (/ (count a-seq) 2))]
[(my-take h a-seq) (my-drop h a-seq)]))


(defn seq-merge [a-seq b-seq]
(if (empty? a-seq)
  b-seq

  (seq-merge (rest a-seq)
  (if (empty? (filter (fn [x] (< x (first a-seq))) b-seq ))
  (concat [(first a-seq)]  b-seq )
  (concat (concat (filter (fn [x] (< x (first a-seq))) b-seq ) [(first a-seq)]) (filter (fn [x] (>= x (first a-seq))) b-seq ))
  ))

  )
  )

(defn merge-sort [a-seq]
(if (empty? a-seq)
  '()
  (if
  (apply < a-seq)
    a-seq
    (let [[y x]
    (halve a-seq)]
      (seq-merge (merge-sort y) (merge-sort x))
      )
    )
  )
  )


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

