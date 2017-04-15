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
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-rec [m a-seq]
  (if (empty? a-seq)
    m
    (max-rec (max m (first a-seq)) (rest a-seq))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max-rec (first a-seq) (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence-rec [longest a-seq]
  (if (empty? a-seq)
    longest
    (longest-sequence-rec (seq-max longest (first a-seq)) (rest a-seq))))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq)        nil
   (empty? (rest a-seq)) (first a-seq)
   :else                 (longest-sequence-rec (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)          false
    (= elem (first a-seq))  true
    :else                   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons
                            (first a-seq)
                            (my-take-while pred? (rest a-seq)))
    :else                  '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    (= (first a-seq) (first b-seq))     (seq= (rest a-seq) (rest b-seq))
    :else                               false))

(defn my-map [f seq-1 seq-2]
  (cond
   (not (or (empty? seq-1) (empty? seq-2)))
     (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2)))
   :else '() ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (< n 2)   n
   (= 2 n)   1
   :else     (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
 (cond
  (> 0 (dec up-to)) '()
  :else             (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else          (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq) '(())
   :else          (cons a-seq (inits (reverse (rest (reverse a-seq)))))))


(defn rotations [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq)                         freqs
    (not (contains? freqs (first a-seq)))  (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
    :else                                  (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map)   '()
   :else            (concat
                      (un-frequencies (rest a-map))
                      (repeat (first (vals a-map)) (first (keys a-map)) ))))

(defn my-take [n coll]
  (cond
   (empty? coll)     '()
   (> n 0)           (cons
                       (first coll)
                       (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll)   '()
   (> n 0)         (my-drop (dec n) (rest coll))
   (<= n 0)        coll))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))
        beg (my-take n a-seq)
        end (my-drop n a-seq)]
    (if (= n 0)
      (vector '() end)
      (vector beg end))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)                      b-seq
   (empty? b-seq)                      a-seq
   (> (first a-seq) (first b-seq))     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   :else                               (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2)   a-seq
   :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))) )))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

