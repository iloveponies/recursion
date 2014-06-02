(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false
    (if (empty? (rest coll)) true
       false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (= (count a-seq) 1)
        (first a-seq) 
        (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
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
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   (not (pred? (first a-seq))) '()))

(defn my-drop-while [pred? a-seq]
  (cond 
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   (not (pred? (first a-seq))) a-seq))

(defn seq= [a-seq b-seq]
  (if (= (count a-seq) (count b-seq))
         (cond
          (and (empty? a-seq) (empty? b-seq)) true
          (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
          :else false)
         false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if 
    (< how-many-times 1) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
    
    

(defn my-range [up-to]
  (if 
    (zero? up-to) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if 
    (empty? a-seq) '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))
  

(comment defn rotations [a-seq]
  (map
     (fn [n _]
       (concat (drop n a-seq) (take n a-seq)))
     (iterate inc 0) a-seq))

(defn rotations-helper [n s-seq]
  (if (empty? s-seq) '(())
    (if (zero? n) nil
    (cons s-seq (rotations-helper (dec n) (concat (rest s-seq) (list (first s-seq))))))))


(defn rotations [a-seq]
    (rotations-helper (count a-seq) a-seq))
   
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [f (first a-seq)]
      (if (contains? freqs f)
          (my-frequencies-helper (assoc freqs f (inc (get freqs f))) (rest a-seq))
          (my-frequencies-helper (assoc freqs f 1) (rest a-seq))))))
    

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) 
    '()
    (let [k (key (first a-map))
          v (val (first a-map))]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll)) '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll) '()
    (if (> n 0) 
      (my-drop (dec n) (rest coll))
      (cons (first coll) (my-drop (dec n) (rest coll))))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))
    

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (< (first a-seq) (first b-seq))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
  (let [halves (halve a-seq)]
    (vec (seq-merge (merge-sort (first halves)) (merge-sort (last halves)))))
    (seq-merge a-seq '())))


(defn monotonic? [a-seq]
          (or (apply <= a-seq) (apply >= a-seq)))

(defn monotonics-helper [a-seq res]
  (if (empty? a-seq) res
    (let [s (last (take-while monotonic? (drop 2 (inits a-seq))))]
      (monotonics-helper (drop (count s) a-seq) (conj res s))))) 
      

(defn split-into-monotonics [a-seq]
  (monotonics-helper a-seq []))


(defn permutations [a-set]
  (if (empty? a-set) '(())     
      (let [r (rotations a-set)]
        (if (= (count a-set) 2) r
            (mapcat (fn [s] (map #(cons (first s) %) (permutations (rest s))))  r)))))

(defn powerset1 [a-set res]
  (if (empty? a-set) (conj res #{})
      (let [fs (first a-set)
            rem (disj a-set fs)
            item (set (list fs))]
        (powerset1 rem (conj
                        v rem)))))

(defn powerset [a-set]
  (let [s (set a-set)]
    (conj (set (mapcat (fn [v] (powerset1 (disj s v) #{})) a-set)) s)))
