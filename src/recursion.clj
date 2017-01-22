(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))


;; (product '(1 2 4))
;;-> (product (cons 1 (cons 2 (cons 4 '()))))

;; (* 1 (product (cons 2 (cons 4 '()))))
;; (* 1 (* 2 (product (cons 4 '()))))
;; (* 1 (* 2 (* 4 (product'()))))

;;-> (* 1 (* 2 (* 4 1)))
;; (* 1 (* 2 4))
;; (* 1 8)
;; 8


(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll)  (first coll)
        :else (my-last (rest coll))))


(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq)  (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

;; (defn longest-sequence [a-seq]
;;   [:-])

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


;; (defn my-filter [pred? a-seq]
;;   [:-])


(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true ;; false
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))



(defn fib [n]
  (cond
    (zero? n) 0
    (zero? (dec n)) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))



(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))



(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (tails a-seq)
         (reverse (rest (reverse (inits a-seq)))))))

;; (defn rotations [a-seq]
;;   (reverse (map concat (tails a-seq) (reverse (rest (reverse (inits a-seq)))))))

;; (defn my-frequencies-helper [freqs a-seq]
;;   (cond
;;     (empty? a-seq)  freqs
;;     (contains? freqs (first a-seq)) (my-frequencies-helper
;;                                      (assoc freqs (first a-seq) (inc (freqs (first a-seq))))
;;                                      (rest a-seq))
;;     :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (if-let [fst (first a-seq)]
    (cond
      (contains? freqs fst) (my-frequencies-helper
                             (assoc freqs fst (inc (freqs fst)))
                             (rest a-seq))
      :else (my-frequencies-helper (assoc freqs (first a-seq) 1)
                                   (rest a-seq)))

    freqs)) 


;; if let? 

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [seq a-map]
  (if (empty? a-map)
    seq
    (un-frequencies-helper 
     (concat (repeat (second (first a-map)) (first (first a-map)))
             seq)
     (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))


(defn my-take-helper [ret n coll]
  (if (or (empty? coll) (zero? n))
    ret
    (my-take-helper (concat ret (list (first coll)))
                    (dec n)
                    (rest coll))))

(defn my-take [n coll]
  (my-take-helper '() n coll))


(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (conj [] (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge-helper [ret a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) ret
        (empty? a-seq) (concat ret b-seq)
        (empty? b-seq) (concat ret a-seq)
        (>= (first b-seq) (first a-seq)) (seq-merge-helper (concat ret (list (first a-seq)))
                                                           (rest a-seq)
                                                           b-seq)
        (< (first b-seq) (first a-seq)) (seq-merge-helper (concat ret (list (first b-seq)))
                                                          a-seq
                                                          (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))



;; (defn merge-sort [a-seq]
;;   (if (or (empty? a-seq) (singleton? a-seq)) a-seq
;;       (let [halves (halve a-seq)]
;;         (seq-merge (merge-sort (first halves))
;;                    (merge-sort (second halves))))))



(defn split-into-monotonics [a-seq]
  )

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])













