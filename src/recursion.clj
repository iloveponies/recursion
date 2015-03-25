(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (and (not (empty? coll))
        (empty? (rest coll))) true
   :else false))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (recur (rest coll))))

(defn max-element [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (max (first coll)
              (max-element (rest coll)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (reduce seq-max nil a-seq))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) '()
   :else (cons (first a-seq)
               (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (recur pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)
        (empty? b-seq)) true
   (or (empty? a-seq)
       (empty? b-seq)) false
   (not (= (first a-seq)
           (first b-seq))) false
   :else (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1)
       (empty? seq-2)) '()
   :else (cons (f (first seq-1)
                  (first seq-2))
               (my-map f
                       (rest seq-1)
                       (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (dec n))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) '()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times)
                          what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) '()
   :else (cons (dec up-to)
               (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons a-seq
               (tails (rest a-seq)))))

(defn inits [a-seq]
  (->> a-seq
       (reverse)
       (tails)
       (map reverse)))

;; New solution inspired by another contributor
(defn rotations [a-seq]
  (set (map concat (tails a-seq) (reverse (inits a-seq)))))

;; Old solution
;; (defn rotations [a-seq]
;;   (defn rothelp [l c]
;;     (cond
;;      (= c (count a-seq)) '()
;;      :else (let [rotated (concat (rest l)
;;                                  (list (first l)))]
;;              (cons rotated
;;                    (rothelp rotated (inc c))))))
;;   (if (empty? a-seq)
;;     '(())
;;     (rothelp a-seq 0)))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq))
     (my-frequencies-helper (assoc freqs
                              (first a-seq)
                              (inc (get freqs (first a-seq))))
                            (rest a-seq))
   :else (my-frequencies-helper
          (assoc freqs (first a-seq) 1)
          (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat (map #(repeat (get a-map %) %) (keys a-map))))

(defn my-take [n coll]
  (map (fn [x _] x) coll (range n)))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (zero? n) coll
   :else (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (cons (take mid a-seq)
          (cons (drop mid a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq)
       (first b-seq)) (concat (list (first a-seq))
                              (seq-merge (rest a-seq) b-seq))
   :else (concat (list (first b-seq))
                 (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (singleton? a-seq) a-seq
   :else (seq-merge (merge-sort (first (halve a-seq)))
                    (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

