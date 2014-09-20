(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (empty? a-seq) false
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1))))) 
         
(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))
    
(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (map (fn [x] x) a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (map reverse (reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
     (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-freq (or (get freqs (first a-seq)) 0)
          new-feqs (assoc freqs (first a-seq) (+ 1 current-freq))]
      (my-frequencies-helper new-feqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (into {} (sort-by val > (my-frequencies-helper {} a-seq))))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [val-times-key (fn [e] (repeat (val e) (key e)))]
      (concat (val-times-key (first a-map)) (un-frequencies (rest a-map)))))) 
  
(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))
    
(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    (apply list coll)
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [limit (int (/ (count a-seq) 2))]
    (vector (my-take limit a-seq) (my-drop limit a-seq))))
      
(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    (apply list a-seq)
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn monotonic? [a-seq] 
  (if (empty? a-seq)
    false
    (or (apply < a-seq) (apply > a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [rest-inits (rest (inits a-seq))
          last-monotonic-init (last (take-while monotonic? rest-inits))
          n (count last-monotonic-init)]
      (cons last-monotonic-init (split-into-monotonics (drop n a-seq))))))


(split-into-monotonics [])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 1 2 1 0])   ;=> ((0 1 2) (1 0))
(split-into-monotonics [0 5 4 7 1 3]) ;=> ((0 5) (4 7) (1 3))



(defn permutations [a-set]
 (let [rot (rotations a-set)]  
   (cond 
     (empty? a-set) '(())
     (singleton? a-set) a-set
     (singleton? (rest a-set)) rot
     :else (let [firsts (map first rot)
                 permrests (map (fn [x-seq] (permutations (rest x-seq))) rot)]
             (apply concat (map (fn [f r] (map (fn [x] (cons f x)) r)) firsts permrests)))))) 

(defn powerset [a-set]
  (if(empty? a-set) 
    #{#{}}
    (let [powerset-rest (powerset (set (rest a-set)))
          sets-with-rest (set (map (fn [r-set] (conj r-set (first a-set))) powerset-rest))]  
      (set (concat powerset-rest (conj sets-with-rest #{(first a-set)} (set a-set)))))))
      

(powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
                      ; #{#{} #{1} #{2} #{1 2} #{4} #{1 4} #{2 4} #{1 2 4}}
(powerset #{})      ;=> #{#{}}
(powerset #{1 2 3 4 5})
(powerset [1 2 3])


(conj #{#{}} #{1 2})
