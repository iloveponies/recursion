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

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else
   (+ (fib (- n 1))
      (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    `()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    `()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    `(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate-seq [a-seq]
  (let [seq-suffix (rest a-seq)
        first-elem (first a-seq)]
    (if (= nil first-elem)
      `()
      (concat seq-suffix (cons first-elem `())))))

; By using map and concat you can merge the tails and inits!
(defn rotations [a-seq]
  (if (empty? a-seq)
    `(())
    (let [tails-list-rev (rest (reverse (tails a-seq)))
          init-list (rest (inits a-seq))]
      (map (fn [l1 l2] (concat l2 l1)) init-list tails-list-rev))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [tmp-count (get freqs (first a-seq))
          real-count (if (= tmp-count nil)
                       0
                       tmp-count)]
      (my-frequencies-helper (assoc freqs (first a-seq) (inc real-count)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    `()
    (let [curr-elem (key (first a-map))
          curr-count (val (first a-map))]
      (concat (repeat curr-count curr-elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll) `()
   (zero? n) `()
   :else (cons (first coll) (my-take (dec n) (rest coll)) )))

(defn my-drop [n coll]
  (cond
   (empty? coll) (rest coll)
   (zero? n) (seq coll)
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div-int (int (/ (count a-seq) 2))
        first-half (my-take div-int a-seq)
        second-half (my-drop div-int a-seq)]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (let [a-first (first a-seq)
          less-than-seq (my-take-while (fn [e] (if (<= e a-first)
                                                        true
                                                        false))
                                              b-seq)
          greater-than-seq (my-drop-while (fn [e] (if (<= e a-first)
                                                           true
                                                           false))
                                                 b-seq)]
      (seq-merge (rest a-seq) (concat less-than-seq (cons a-first `()) greater-than-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
    a-seq
    (let [first-half (get (halve a-seq) 0)
          second-half (get (halve a-seq) 1)]
      (seq-merge (merge-sort first-half)
                 (merge-sort second-half)))))

(defn monotonic-seq? [a-seq]
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

