(ns recursion)

; Evaluation of product for [1 2 3]
;=> (product [1 2 3])
;=> (* 1 (product (cons 2 (cons 3 ))))
;=> (* 1 (* 2 (product (cons 3 ))))
;=> (* 1 (* 2 (* 3 (product '()))))      ; empty -> 1 is returned
;=> (* 1 (* 2 (* 3 1)))
;=> (* 1 (* 2 3))
;=> (* 1 6)
;=> 6
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (let [last-element (fn[first-element next-element] (if (nil? next-element) first-element next-element))]
    (if (empty? coll)
      nil
      (last-element (first coll) (my-last (rest coll))))))

(defn max-element [a-seq]
  (let [max-value (fn[first-element next-element] (if (nil? next-element) first-element (max first-element next-element)))]
    (if (empty? a-seq)
      nil
      (max-value (first a-seq) (max-element (rest a-seq))))))

(defn rec-seq-max [seq-1 seq-2]
  (let [max-sequence? (fn[] (if (> (first seq-1) (first seq-2)) true false))]
    (cond
      (empty? seq-1) false
      (empty? seq-2) true
      (and (empty? (rest seq-1)) (rest seq-2)) (max-sequence?)
      :else (rec-seq-max (rest seq-1) (rest seq-2)))))

(defn seq-max [seq-1 seq-2]
  (if (rec-seq-max seq-1 seq-2) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (let [auto-reject (fn[x] false)]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons (first a-seq) (my-drop-while auto-reject (rest a-seq))))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

; Evaluation of power 2^3
;=> (power 2 3)
;=> (* 2 (power (cons 2 (cons 1 ))))
;=> (* 2 (* 2 (power (cons 1 ))))
;=> (* 2 (* 2 (* 2 (power 0))))      ; 0 -> 1 is returned
;=> (* 2 (* 2 (* 2 1)))
;=> (* 2 (* 2 2))
;=> (* 2 4)
;=> 8
(defn power [n k]
  (if (== 0 k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if(< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if(<  up-to 1)
    ()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (seq (set (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [update-freq (fn[x]
                      (if (contains? freqs x)
                        (assoc freqs x (+ 1 (get freqs x)))
                        (assoc freqs x 1)))]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (update-freq (first a-seq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [repeat-key (fn [[key key-count]] (repeat key-count key))]
  (if (empty? a-map)
    ()
    (concat (repeat-key (first a-map)) (un-frequencies(rest a-map))))))

(defn my-take [n coll]
  (if (or (<  n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (> n 0) (my-drop (dec n) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [seq-length (int (/ (count a-seq) 2))]
    (if (== 0 seq-length)
      (vector () a-seq)
      (vector (my-take seq-length a-seq) (my-drop seq-length a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    ))

(defn merge-sort [a-seq]
  (let [rec-merge (fn [[first-half second-half]]
                    (seq-merge (merge-sort first-half) (merge-sort second-half)))]
    (if (nil? (second a-seq))
      a-seq
      (rec-merge (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

