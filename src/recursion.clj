(ns recursion)

(defn product [coll] (if (empty? coll)
                       1
                       (* (first coll) (product (rest coll)))))
; (product [1 2 4])
; (* 1 (product [2 4]))
; (* 1 2 (product [4]))
; (* 1 2 4)
; => 8


(defn singleton? [coll] (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq] (if (not (or (singleton? a-seq) (empty? a-seq)))
                            (max (first a-seq) (max-element (rest a-seq)))
                            (first a-seq)))

(defn seq-max [seq-1 seq-2] (if (< (count seq-2) (count seq-1)) seq-1 seq-2))

(defn longest-sequence [a-seq] (if (not (or (singleton? a-seq) (empty? a-seq)))
                                 (seq-max (first a-seq) (longest-sequence (rest a-seq)))
                                 (first a-seq)))

(defn my-filter [pred? a-seq] (if (empty? a-seq)
                                a-seq
                                (if (pred? (first a-seq))
                                  (cons (first a-seq) (my-filter pred? (rest a-seq)))
                                  (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq] (if (empty? a-seq)
                                        false
                                        (if (= elem (first a-seq))
                                          true
                                          (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq] (if (and (not (empty? a-seq)) (pred? (first a-seq)))
                                    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
                                    '()))

(defn my-drop-while [pred? a-seq] (if (and (not (empty? a-seq)) (pred? (first a-seq)))
                                    (my-drop-while pred? (rest a-seq))
                                    a-seq))

(defn seq= [a-seq b-seq] (if (= (count a-seq) (count b-seq))
                           (if (and (empty? a-seq) (empty? b-seq))
                             true
                             (if (= (first a-seq) (first b-seq))
                               (seq= (rest a-seq) (rest b-seq))
                               false))
                           false))

(defn my-map [f seq-1 seq-2] (if (or (empty? seq-1) (empty? seq-2))
                               '()
                               (if (or (singleton? seq-1) (singleton? seq-2))
                                 (cons (f (first seq-1) (first seq-2)) '())
                                 (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k] (if (< 0 k)
                    (* n (power n (dec k)))
                    1))

(defn fib [n] (if (or (== n 0) (== n 1))
                (if (== n 0)
                  0
                  1)
                (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat] (if (< 0 how-many-times)
                                                  (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
                                                  '()))

(defn my-range [up-to] (if (< 0 up-to)
                         (cons (dec up-to) (my-range (dec up-to)))
                         '()))

(defn tails [a-seq] (if (empty? a-seq)
                      (cons '() '())
                      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq] (if (empty? a-seq)
                      (cons '() '())
                      (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn my-rotations-helper [n a-seq] (if (< 0 n)
                                      (cons
                                       (concat (rest a-seq) (cons (first a-seq) '()))
                                       (my-rotations-helper (dec n) (concat (rest a-seq) (cons (first a-seq) '()))))
                                      '()))

(defn rotations [a-seq] (if (empty? a-seq)
                          (cons '() '())
                          (my-rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq] (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper-1 [amount symb] (if (< 0 amount)
                                            (cons symb (un-frequencies-helper-1 (dec amount) symb))
                                            '()))

(defn un-frequencies-helper-2 [a-map key-seq a-seq] (if (empty? key-seq)
                                                a-seq
                                                (un-frequencies-helper-2 a-map (rest key-seq) (concat a-seq (un-frequencies-helper-1 (get a-map (first key-seq)) (first key-seq))))))

(defn un-frequencies [a-map] (un-frequencies-helper-2 a-map (keys a-map) '()))

(defn my-take [n coll] (if (and (< 0 n) (not (empty? coll)))
                         (cons (first coll) (my-take (dec n) (rest coll)))
                         '()))

(defn my-drop [n coll] (if (and (< 0 n) (not (empty? coll)))
                         (my-drop (dec n) (rest coll))
                         coll))

(defn halve [a-seq] (let [half-way (int (/ (count a-seq) 2))]
                      (conj '() (my-drop half-way a-seq) (my-take half-way a-seq))))

(defn seq-merge [a-seq b-seq] (if (and (empty? a-seq) (empty? b-seq))
                                '()
                                (if (or (empty? a-seq) (empty? b-seq))
                                  (if (empty? a-seq)
                                    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
                                    (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
                                  (if (< (first a-seq) (first b-seq))
                                    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
                                    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq] (if (or (= 1 (count a-seq)) (= 0 (count a-seq)))
                           (if (= 1 (count a-seq))
                              (cons (first a-seq) '())
                              '())
                           (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (first (rest (halve a-seq)))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

