(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) 
    nil
    (if (empty? (rest a-seq)) 
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) 
    nil
    (if (empty? (rest a-seq)) 
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) 
     false
   (not= elem (first a-seq)) 
     (sequence-contains? elem (rest a-seq))
   :else
     true))

(defn my-take-while [pred? a-seq]
  (cond 
   (empty? a-seq)
     a-seq
   (pred? (first a-seq)) 
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     []))

(defn my-drop-while [pred? a-seq]
  (cond 
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    []
    (cons 
     (f (first seq-1) (first seq-2)) 
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== n 1) 1
   (== n 0) 0
   (== k 0) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond 
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
   (< how-many-times 1) []
   :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    [a-seq]    
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [f (fn g [n xs] (if (< n 1) []
                        (cons xs (g (- n 1) (concat (rest xs) [(first xs)])))))]
    (if (empty? a-seq) [[]] (f (count a-seq) a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [fst (first a-seq)]
   (if (empty? a-seq) 
     freqs
     (my-frequencies-helper (assoc freqs fst
       (if (contains? (set (keys freqs)) fst) (inc (get freqs fst)) 1)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [f (fn [k] (repeat (get a-map k) k))]
    (apply concat (map f (keys a-map)))))

(defn my-take [n coll]
  (cond
   (== n 0) []
   (empty? coll) []
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq))) 
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [f (fn [x] (apply < x))
        g (fn [x] (apply >= x))
        inc-list (last (filter f (rest (inits a-seq))))
        dec-list (last (filter g (rest (inits a-seq))))
        monotonic (if (> (count inc-list) (count dec-list)) inc-list dec-list)]
     (if (empty? a-seq) 
       a-seq 
       (concat [monotonic] (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-seq]
  (let [a-set (set a-seq)]
    (cond (empty? a-set) [[]]
          (empty? (rest a-set)) a-set
          :else (for [elem a-set
                      perms (permutations (clojure.set/difference a-set #{elem}))] 
                  (cons elem (if (coll? perms) perms [perms]))))))

(defn powerset [a-set]
  [:-])