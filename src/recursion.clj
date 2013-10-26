(ns recursion)

(defn product [coll]
  (if
      (empty? coll) 1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (= (first coll) nil)) 
   (= (first (rest coll)) nil)))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (empty? (rest coll)) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn longer-seq [seq-1 seq-2]
  (cond
   (empty? seq-1) 2
   (empty? seq-2) 1
   :else (longer-seq (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (let [longer (longer-seq seq-1 seq-2)]
    (case longer
      1 seq-1
      2 seq-2
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(defn my-filter [pred? a-seq]
  (if
      (empty? a-seq) a-seq
      (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

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
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else  (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (== up-to 0) '()
   (== up-to 1) '(0)
   :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond 
   (empty? a-seq) [()]
   :else (cons a-seq             
               (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond 
   (empty? a-seq) [()]
   :else (cons a-seq             
               (inits (butlast a-seq)))))

(defn rotations-helper [iteration a-seq]
  (let [cnt (count a-seq)]
    (if
     (== cnt iteration) '()
     (cons (concat (take-last (- cnt iteration) a-seq) (take iteration a-seq))
                 (rotations-helper (+ iteration 1) a-seq)))))
         
(defn rotations [a-seq]
  (if 
      (empty? a-seq) '(())
      (rotations-helper 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if
      (empty? a-seq) freqs
      (my-frequencies-helper 
       (assoc freqs (first a-seq) (+ (or (get freqs (first a-seq)) 0) 1)) 
       (rest a-seq))))

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [key] (repeat (get a-map key) key)) (keys a-map)))

(defn my-take [n coll]
  (if
   (or (empty? coll) (== n 0)) '()
   (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if
   (or (empty? coll) (== n 0)) coll
   (my-drop (- n 1) (rest coll))))
 
(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (or (empty? b-seq) (and (not (empty? a-seq)) (< first-a first-b))) (cons first-a (seq-merge (rest a-seq) b-seq))
     :else (cons first-b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [[h1 h2] (halve a-seq) ]
    (cond
     (or (empty? a-seq) (singleton? a-seq)) a-seq
     :else (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [seq] 
                      (cond
                       (empty? seq) false
                       :else (or (apply <= seq) (apply >= seq))))
        monos (first (filter monotonic? (inits a-seq)))]
    (if
        (empty? a-seq) '()
        (cons monos
              (split-into-monotonics (drop (count monos) a-seq))))))

(defn permutations [a-set]
  (cond 
   (empty? a-set) '(()) 
   (singleton? a-set) (list a-set)
   :else (let [s (set a-set)]
           (for [head s
                 tail (permutations (disj s head))]
             (cons head tail)))))

(defn powerset [a-set]
  (if (empty? a-set) '(())
      (let [prev (powerset (rest a-set))]
        (concat 
         (map (fn [elt] (cons (first a-set) elt)) prev) 
         prev))))

