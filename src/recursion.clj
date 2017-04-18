(ns recursion)

(defn product [coll]
    (cond
        (empty? coll) 1
        :else (* (first coll) (product (rest coll)))
        ))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
    (cond
        (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))
        ))

(defn max-element [a-seq]
    (cond
        (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))
        ))

(defn seq-max [seq-1 seq-2]
  (let [seq-1-len (count seq-1)
        seq-2-len (count seq-2)]
        (if (> seq-1-len seq-2-len) seq-1 seq-2)))

(defn longest-sequence [a-seq]
    (cond
        (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
    (cond
        (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))
    ))

(defn sequence-contains? [elem a-seq]
    (cond
        (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
    (cond
        (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()
    ))

(defn my-drop-while [pred? a-seq]
    (cond
        (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq)) ; drop from returned seq
        :else a-seq ; with false, return rest of elements
    ))

(defn seq= [a-seq b-seq]
    (cond
        (and (not (empty? a-seq)) (empty? b-seq)) false ; need xor operator
        (and (empty? a-seq) (not (empty? b-seq))) false ; need xor operator
        (and (empty? a-seq) (empty? b-seq)) true ; need xor operator
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))
    ))

(defn my-map [f seq-1 seq-2]
    (cond
        (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
    (cond
        (= 0 n) 0
        (= 0 k) 1
        :else (* n (power n (dec k)))
    ))

(defn fib [n]
    (cond
        (= 0 n) 0
        (= 1 n) 1
        :else (+ (fib (dec (dec n))) (fib (dec n)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
    (let [some-var what-to-repeat]  ; i don't know why this is required, but you need to pput what-to-repeat into a local var
                                    ; you can't use it directly eg, '(what-to-repeat) breaks but '("this works")
    (cond
        (<= how-many-times 0) '()
        (= 1 how-many-times) (seq [some-var])
        :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    )))

(defn my-range [up-to]
    (cond
        (= 0 up-to) '()
        (= 1 up-to) (seq [0])
        :else (cons (dec up-to) (my-range (dec up-to)))
    ))

; remove n-items from the front of a sequence
(defn remove-n [n a-seq]
    (cond
        (= 0 n) a-seq
        :else (remove-n (dec n) (rest a-seq))
    ))

(defn tails [a-seq]
    (map-indexed (fn [index item] (remove-n index a-seq)) (cons '() a-seq)))
    ; could not figure out how to do it without map-indexed

(defn inits [a-seq]
    (map-indexed (fn [index item] (reverse (remove-n index (reverse a-seq)))) (cons '() a-seq)))
    ; could not figure out how to do it without map-indexed

(defn shift-seq-by-n [n a-seq]
    (cond
        (= 0 n) a-seq ; base case: stop shifting
        :else (shift-seq-by-n (dec n) (cons (my-last a-seq) (drop-last a-seq))) ; get last element, remove last element, add last element to first pos
    ))

(defn rotations [a-seq]
    ; use my-last to get last element, use cons to put it in front
    ; repeat n times
    (cond
        (empty? a-seq) '(())
        :else (map-indexed (fn [index item] (shift-seq-by-n index a-seq)) a-seq)
    ))

(defn my-frequencies-helper [freqs a-seq]
(if (empty? a-seq)
    freqs ; empty, done with recursion
    (let [first-item (first a-seq) ; not empty, still more recursion to do
          new-freqs freqs]
        (if (contains? freqs first-item) ; check if passed in map contains next element in seq
            (my-frequencies-helper (assoc new-freqs first-item (inc (get freqs first-item))) (rest a-seq)); contains already, so increment its value
            (my-frequencies-helper (assoc new-freqs first-item 1) (rest a-seq)); does not contain, add new
        )
    ))
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

; (un-frequencies {:a 3 :b 2 "^_^" 1}) ;=> (:a :a :a "^_^" :b :b)
(defn un-frequencies [a-map]
    (let [[freq-key freq-val] (first a-map)]
        (if (empty? a-map)
        '()
        (concat (repeat freq-val freq-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
        '()
        (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (remove-n n coll)) ; don't reinvent the wheel

(defn halve [a-seq]
  (let [seq-len (count a-seq)
        halve-1-len (int (/ seq-len 2))]
  (cond
      (empty? a-seq) '(())
      (singleton? a-seq) (cons '() (cons a-seq '()))
      :else (cons (my-take halve-1-len a-seq) (cons (my-drop halve-1-len a-seq) '())))))

(defn compare-merge-thing [a-seq b-seq]
    ; (first a-seq) is the current pointer
    (cond
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (compare-merge-thing (rest a-seq) b-seq)) ; a[0] < b[0], so emit a and pointer goes to b
        :else (cons (first b-seq) (compare-merge-thing (rest b-seq) a-seq))))

(defn seq-merge [a-seq b-seq]
        (if (< (first a-seq) (first b-seq))
            (cons (first a-seq) (compare-merge-thing b-seq (rest a-seq))) ; a < b, add a, pointer goes to b
            (cons (first b-seq) (compare-merge-thing a-seq (rest b-seq))))) ; b < a, add b, pointer goes to a

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq ; already sorted
    (let [[lhs rhs] (halve a-seq)](seq-merge (merge-sort lhs) (merge-sort rhs)))
  ))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
