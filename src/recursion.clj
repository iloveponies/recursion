(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) []
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))
   ))

(defn my-take-while [pred? a-seq]
  (cond
   (or (empty? a-seq) (not (pred? (first a-seq)))) []
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))
   ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (empty? b-seq) (not (empty? a-seq))) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= 0 k) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (<= up-to 0) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) [a-seq]
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq)))
  )

(defn do-n-rotation [a-seq n]
  (cond
   (empty? a-seq) [[]]
   (= n 0) []
   :else (let [shifted-one (concat (rest a-seq) [(first a-seq)])]
     (cons shifted-one (do-n-rotation shifted-one (dec n))))))

(defn rotations [a-seq]
  (do-n-rotation a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-value (first a-seq)
          first-value-freq (if (contains? freqs first-value) (get freqs first-value) 0)]
      (my-frequencies-helper (assoc freqs first-value (inc first-value-freq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper '{} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[sym, freq] (first a-map)]
      (concat (repeat freq sym) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll) []
   (= 0 n) []
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) []
   (= 0 n) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (cons (my-take mid a-seq) [(my-drop mid a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a-min (first a-seq)
               b-min (first b-seq)]
     (if (<= a-min b-min)
       (cons a-min (seq-merge (rest a-seq) b-seq))
       (cons b-min (seq-merge a-seq (rest b-seq)))
       ))))

(defn merge-sort [a-seq]
  (cond
   (or (empty? a-seq) (singleton? a-seq)) a-seq
   :else (let [split-pair (halve a-seq)]
    (seq-merge (merge-sort (first split-pair)) (merge-sort (second split-pair))))))

; hmm, busted. i'll get back to this.
(defn pull-first-monotonic-helper [a-seq]
    (let [all-inits (inits a-seq)
          all-monotone-inits (apply (fn [x] (apply <= x)) all-inits)]
      (first all-monotone-inits)
      ))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq) []
   :else (let [first-seq (pull-first-monotonic-helper a-seq)
               first-seq-length (count first-seq)]
           (cons first-seq (split-into-monotonics (my-drop first-seq-length a-seq)))
           )))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  (cond
   (empty? a-set) #{a-set}
   :else (let [el (first a-set)
               rest-powerset (powerset (rest a-set))]
          (clojure.set/union
           rest-powerset
           (map (fn [x] (conj x el)) rest-powerset))
          )))

