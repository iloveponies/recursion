(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll))) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) 
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [rec-max (if (empty? seq-1) 2
                  (if (empty? seq-2) 1
                    (seq-max (rest seq-1) (rest seq-2))))]
    (if (= rec-max 1) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq))))) 

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) 
      (cons (first a-seq)(my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) []
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (== (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
 (cond
  (or (empty? seq-1) (empty? seq-2)) '()
  :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1
    (if (== k 1)
      n
      (* n (power n (dec k))))))

(defn fib [n]
  (cond 
   (zero? n) 0
   (== n 1) 1
   :else (+ (fib (- n 1))
            (fib (- n 2)))))

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
   (empty? a-seq) ['()]
   :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
 (map reverse (tails (reverse a-seq))))



(defn rotate [a-seq] 
  (if (empty? a-seq)
    '()
    (concat (rest a-seq) (cons (first a-seq) '()))))

(defn rotations [a-seq]
 (let [rotations-helper (fn ! [b-seq]
                          (if (= a-seq b-seq)
                            (cons b-seq '())
                            (concat (cons b-seq '()) (! (rotate b-seq)))))]
   (rotations-helper (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper (assoc-in freqs [(first a-seq)] (inc (get-in freqs [(first a-seq)]))) (rest a-seq))
   :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elem (first a-map)
        first-key (first elem)
        first-val (second elem)]
    (cond
      (empty? a-map) []
      :else (concat (repeat first-val first-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (zero? n) []
   (>= n (count coll)) (cons (first coll) (my-take (dec (count coll)) (rest coll)))
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (or (empty? coll) (zero? n)) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq  (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (< (count a-seq) 2) a-seq
   :else (let [[first-half second-half] (halve a-seq)]
           (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])