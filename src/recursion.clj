(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (not (empty? coll)) (empty? (rest coll)))
   true
   false))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (max-element (cons (max (nth a-seq 0) (nth a-seq 1)) (rest (rest a-seq)))))))
 

(defn seq-max [seq-1 seq-2]
  (cond 
    (> (count seq-1) (count seq-2)) seq-1
    :else seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (longest-sequence (cons (seq-max (nth a-seq 0) (nth a-seq 1)) (rest (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))
  


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
  (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
      1
      (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    ( == n 1) 1
    :else (+ (fib (- n 2)) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (conj () ())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [test (fn [x] (take (dec (count x)) x))]
    (if (empty? a-seq)
      (conj () ())
      (cons (seq a-seq) (inits (test a-seq))))))

(defn rotations [a-seq]
 (let [lll (fn [a] (if (not (vector? (first a))) (concat (vector (vector (count a-seq)) 0) a-seq) a-seq))]
   (if (and (vector? (first a-seq)) (= (second a-seq) (ffirst a-seq)))
     ()
     (if (= (count a-seq) 0)
       (cons () ())
       (cons (seq (nthrest (lll a-seq) 2)) (rotations (concat (concat (vector (first (lll a-seq)) (inc (second (lll a-seq)))) (nthrest (lll a-seq) 3)) (vector (nth (lll a-seq) 2)))))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (get freqs  (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs  (first a-seq)))) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))  

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (concat (seq (repeat (second (first a-map)) (ffirst a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (reverse (nthrest (reverse coll) (- (count coll) n))))

(defn my-drop [n coll]
  (nthrest coll n))

(defn halve [a-seq]
  (let [ind (int (/ (count a-seq) 2))]
  (vector (my-take ind a-seq) (my-drop ind a-seq))))
  
(defn sij [ele b-seq bb n]
  (if (and (apply <= bb) (> n 0))
    bb
    (sij ele b-seq (concat (my-take n b-seq) (vector ele) (my-drop n b-seq)) (inc n))))
 
(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (seq-merge (rest a-seq) (sij (first a-seq) b-seq b-seq 0))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq 
   (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn inits2 [a-seq n]
  (if (= (inc (count a-seq)) n)
    ()
  (cons (reverse (nthrest (reverse a-seq) (- (count a-seq) n))) (inits2 a-seq (inc n))))) 
  
(defn insp [a-seq n m]
  (if (>= (+ n 1) (count a-seq))
    a-seq
    (cond
      (and (= m -1) (< (nth a-seq n) (nth a-seq (inc n)))) (insp a-seq (inc n) 0)
      (and (= m -1) (> (nth a-seq n) (nth a-seq (inc n)))) (insp a-seq (inc n) 1)
      (and (= m -1) (= (nth a-seq n) (nth a-seq (inc n)))) (insp a-seq (inc n) -1)
      (and (= m 0) (<= (nth a-seq n) (nth a-seq (inc n)))) (insp a-seq (inc n) 0)
      (and (= m 1) (>= (nth a-seq n) (nth a-seq (inc n)))) (insp a-seq (inc n) 1)
      :else (my-take (inc n) a-seq))))
      
(defn split-into-monotonics [a-seq]
  (let [uu (insp a-seq 0 -1)]
  (if (= (count a-seq) 1)
    (seq [a-seq])
    (if (= (count a-seq) 0)
      ()
    (cons uu (split-into-monotonics (my-drop (count uu) a-seq)))))))
  
(defn permutate [a-set aa n1 n2 len]
  (let [xx (nth a-set n1)]
  (if (> n2 len)
    ()
    (cons aa (permutate a-set (assoc (assoc a-set n1 (get a-set n2)) n2 xx) n1 (inc n2) len)))))
    
(defn workonp [a-set n1 n2]
  (let [pmp (fn [] (permutate a-set a-set n1 n2 (count a-set)))]
  (if (= n1 (count a-set))
    ()
    (cons (pmp) (workonp a-set (inc n1) n2)))))
    
    
(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

