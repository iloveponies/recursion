(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;   (product '(1 2 4))
;=  (product (cons 1 (cons 2 (cons 4 '()))))
;=> (* 1 (product (cons 2 (cons 4 '()))))
;=> (* 1 (* 2 (product (cons 4 '()))))
;=> (* 1 (* 2 (* 4 (product '()))))
;=> (* 1 (* 2 (* 4 1))))        ; (empty? '()) is true, so (product '()) ;=> 1
;=> (* 1 (* 2 4)))
;=> (* 1 8))
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (my-last (rest a-seq))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)
    ))

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
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) true
    (not= (count seq-1) (count seq-2)) false
    (not= (first seq-1) (first seq-2)) false
    :else (seq= (rest seq-1) (rest seq-2))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1)   1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (map reverse (reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [token (first a-seq)
          new-freqs (if (contains? freqs token)
                      (assoc freqs token (inc (freqs token)))
                      (assoc freqs token 1)
                      )]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [pair (first a-map)
          new-seq (concat a-seq (repeat (second pair) (first pair)))]
      (un-frequencies-helper new-seq (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-length (count a-seq)
        first-half (int (/ seq-length 2))]
    [(my-take first-half a-seq) (my-drop first-half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (singleton? a-seq) a-seq
    :else (apply seq-merge (map merge-sort (halve a-seq)))))

(defn my-take-while-pair [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (singleton? a-seq) a-seq
    (pred? (first a-seq) (second a-seq))
      (concat [(first a-seq)] (my-take-while-pair pred? (rest a-seq)))
    :else [(first a-seq)]))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [inc-monotonic (my-take-while-pair < a-seq)
          dec-monotonic (my-take-while-pair > a-seq)
          monotonic (if (or (empty? inc-monotonic) (singleton? inc-monotonic))
                      dec-monotonic
                      inc-monotonic)
          monotonic-length (count monotonic)
          rest-seq (drop monotonic-length a-seq)]
      (concat [monotonic] (split-into-monotonics rest-seq)))))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [first-token (first a-seq)
          rest-perms (permutations (rest a-seq))
          seqs (map (fn [x] (cons first-token x)) rest-perms)]
      (apply concat (map rotations seqs)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [rest-sets (powerset (rest a-set))
          first-token (first a-set)
          sets (map (fn [x] (set (cons first-token x))) rest-sets)]
      (set (concat sets rest-sets)))))

