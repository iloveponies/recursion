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

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
   (<= (count seq-1) (count seq-2)) seq-2
   :else seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (every? (complement empty?) [a-seq b-seq])
     (if (= (first a-seq) (first b-seq))
       (seq= (rest a-seq) (rest b-seq))
       false)
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== k 0) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? (rest a-seq))
    (cons (map identity a-seq) '())
    (let [iter2 (fn iter [a-seq2]
                 (let [rotated-seq (concat (rest a-seq2) (cons (first a-seq2) '()))]
                   (if (= a-seq rotated-seq)
                     '()
                      (cons rotated-seq (iter rotated-seq)))))]
      (cons (map identity a-seq) (iter2 a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)]
      (if (contains? freqs first-element)
        (my-frequencies-helper (assoc freqs first-element (inc (get freqs first-element))) (rest a-seq))
        (my-frequencies-helper (assoc freqs first-element 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[a-key a-count]] (repeat a-count a-key)) a-map)))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (== n 0) coll
   (empty? coll) '()
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-length (count a-seq)
        first-half-length (int (/ seq-length 2))]
    [(my-take first-half-length a-seq)
     (my-drop first-half-length a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq))
     (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq ))
   :else
     (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    (into '()  a-seq)
    (apply seq-merge (map merge-sort (halve a-seq)))))

;; (defn first-mono-helper2 [opt mono-seq part-seq]
;;   (cond
;;    (empty? part-seq) [mono-seq part-seq]
;;    (opt (first mono-seq) (first part-seq))
;;      (first-mono-helper2 opt
;;                          (cons (first part-seq) mono-seq)
;;                          (rest part-seq))
;;     :else [mono-seq part-seq]))


;; (defn first-mono [opt a-seq]
;;   (let [[mono-seq part-seq] (first-mono-helper2 opt [(first a-seq)] (rest a-seq))]
;;     [(reverse mono-seq) part-seq]))

;; (defn split-into-monotonics [a-seq]
;;   (cond
;;    (empty? a-seq) '()
;;    (empty? (rest (rest a-seq))) (into '() [a-seq])
;;     :else (cond
;;             (<= (first a-seq) (get a-seq 1))
;;               (let [[mono part-seq] (first-mono <= a-seq)]
;;               (concat [mono] (split-into-monotonics part-seq)))
;;             :else
;;               (let [[mono part-seq] (first-mono > a-seq)]
;;               (concat [mono] (split-into-monotonics part-seq))))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    false
    (or (apply <= a-seq) (apply > a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono (last (filter monotonic? (inits a-seq)))]
      (cons mono (split-into-monotonics (drop (count mono) a-seq))))))



(defn permutations [a-set]
  (cond
    (empty? a-set) (cons '() '())
    (empty? (rest a-set)) (cons a-set '())
   :else (let [helper (fn helper [a-set-part]
                 (if (empty? a-set-part)
                   '()
                    (let [first-element (first a-set-part)]
                      (concat (map (fn [seq2] (cons first-element seq2)) (permutations (disj (set a-set) first-element)))
                         (helper (rest a-set-part))))))]
    (helper a-set))))


(defn nchoose2 [a-set]
   (cond
     (empty? (rest a-set)) '()
     (== (count a-set) 2) (cons (set a-set) '())
     :else (let [first-elem (first a-set)]
                 (concat (map (fn [elem] #{first-elem elem}) (rest a-set)) (nchoose2 (rest a-set))))))

(defn nchoose3 [a-set k]
  (cond
   ;(== k 0) (cons #{} '())
   ;(== k 1) (map (fn [elem] #{elem}) a-seq)
   (== (count a-set) k) (cons (set a-set) '())
   (== k 0) (cons #{} '())
   (== k 1) (map (fn [elem] #{elem}) a-set)
   (== k 2) (nchoose2 a-set)
   :else (let [helper (fn helper [a-set k]
                        (if (== (count a-set) k)
                          (cons (set a-set) '())
                          (let [first-elem (first a-set)]
                          (concat (map (fn [subset] (conj subset first-elem)) (nchoose3 (rest a-set) (dec k)))
                                  (helper (rest a-set) k)))))]
           (helper a-set k))))

(defn powerset [a-set]
  (let [helper (fn helper [k] (nchoose3 a-set k))]
    (set (apply concat (map helper (range (inc (count a-set))))))))

