;; Some solutions here are borrowed from github user Joyfolk

(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

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
         (let [max-value (max-element (rest a-seq))]
           (if (nil? max-value)
             0
             max-value)))))

;; (max-element [1 3 4 5])
;; (max (1 (max-element [3 4 5])))
;;   (max 1 (max 3 (max-element [4 5])))
;;     (max 1 (max 3 (max 4 (max-element [5])))
;;       (max 1 (max 3 (max 4 5)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]  
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    (list)
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (count a-seq)(count b-seq)) ) false
    (and (empty? a-seq) (empty? b-seq)) true
    (and (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq))
    :else
    false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (> how-many-times 0) (cons what-to-repeat
                               (my-repeat (dec how-many-times) what-to-repeat))
    :else '()))

(defn my-range [up-to]
  (cond
    (> up-to 0) (cons (dec up-to)
                      (my-range (dec up-to)))
    :else '()))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons (seq a-seq)
                (tails (rest a-seq)))))


(defn inits [a-seq]
  (reverse (map reverse ( tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)
          end-of-seq (rest a-seq)
          found-count (or (freqs first-element) 0)]
      (my-frequencies-helper (assoc freqs first-element (inc found-count))
                             end-of-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;; (defn un-frequencies [a-map]
;;   (if (empty? a-map)
;;     '(())
;;     (let [first-element (first a-map)
;;         chars (first first-element)
;;         count (second first-element)]
;;       (apply conj (un-frequencies (rest a-map))
;;              (repeat count chars)
;;              ))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '(())
    (apply concat (map (fn [[x c]]
                         (repeat c x))
                       a-map))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (conj (my-take (dec n) (rest coll))
          (first coll))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [limiter (int (/ (count a-seq) 2))]
    [(my-take limiter a-seq) (my-drop limiter a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (let [a-first (first a-seq)
          b-first (first b-seq)]
      (if (>= a-first b-first)
        (conj (seq-merge a-seq (rest b-seq))
              b-first)
        (conj (seq-merge b-seq (rest a-seq))
                    a-first)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[left, right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

