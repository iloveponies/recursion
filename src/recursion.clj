(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :default (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2    
    ))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :default (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :default (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :default (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :default '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :default a-seq)) ;pred? false for first -> quit dropping

(defn seq= [a-seq b-seq]
  (cond
   (not (= (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :default false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :default (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
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
  (map reverse (tails (reverse a-seq))))
 
(defn n-rotate [n a-seq] 
  (let [once-rotated (concat (rest a-seq) (cons (first a-seq) nil))]
    (cond
     (= n 0) a-seq
     (= n 1) once-rotated
     :default (n-rotate (dec n) once-rotated))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [n] (n-rotate n a-seq)) (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [first-val (first a-seq)        
        new-count (if (contains? freqs first-val)
                    (inc (get freqs first-val))
                    1)
        new-freqs (assoc freqs first-val new-count)]

    (if (empty? a-seq)
      freqs
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-freq-pair [[token count]]
  (repeat count token)
)

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) nil
   :default (concat (un-freq-pair (first a-map)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()    
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    (vector (my-take div a-seq) (my-drop div a-seq))))

(defn seq-merge-helper [prefix a-seq b-seq]
  (cond
   (empty? a-seq) (concat prefix b-seq)
   (empty? b-seq) (concat prefix a-seq)
   (< (first a-seq) (first b-seq)) (seq-merge-helper (concat prefix (cons (first a-seq) nil)) (rest a-seq) b-seq)
   :default (seq-merge-helper (concat prefix (cons (first b-seq) nil)) a-seq (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (let [first-half (first (halve a-seq))
        second-half (second (halve a-seq))]
    (if (or (empty? a-seq) (singleton? a-seq))
      a-seq
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

