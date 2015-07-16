(ns recursion)

(defn product [coll]
  (apply * coll))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (last coll))

(defn max-element [a-seq]
  (cond 
    (empty? a-seq) nil
    :else
      (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
(if (empty? a-seq) nil  
(nth a-seq (first (apply max-key (fn [x] (count (second x)) )  (map-indexed vector a-seq)))))
)


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
     a-seq
     (if (pred? (first a-seq))
         (cons (first a-seq) (my-filter pred? (rest a-seq)))
         (my-filter pred? (rest a-seq))))
)


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      	true
	(sequence-contains? elem (rest a-seq))))
)



(defn my-take-while [pred? a-seq]

  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        ()))
)


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred?  (first a-seq))
        (my-drop-while pred? (rest a-seq))
        a-seq))
)


(defn seq= [a-seq b-seq]
  (if (not (= (count a-seq) (count b-seq)))
    false	   
    (if (and (empty? a-seq) (empty? b-seq))
      true
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
	false)))
)

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2))
    	()
      	(cons (f (first seq-1) (first seq-2))
	      (my-map f (rest seq-1)(rest seq-2))))
 
)

(defn power [n k]
  (if (= 0 k)
      1
      (* n (power n (- k 1))))
)

(defn fib [n]
  (cond 
  	(= 0 n) 0
	(= 1 n) 1
	:else (+ (fib (- n 1)) (fib (- n 2))))
)

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
  	(>= 0 how-many-times) ()
	:else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat) ))
)

(defn my-range [up-to]
  (cond 
  	(>= 0 up-to) ()
	:else (cons (- up-to 1) (my-range (- up-to 1))))
)


(defn tails [a-seq]
  (cond
        (= 0 (count a-seq)) [[]]
        :else (into [a-seq] (tails (rest a-seq))))
)


(defn inits [a-seq]
  (cond
        (= 0 (count a-seq)) [[]]
        :else (into [a-seq] (inits (pop a-seq))))

)
(defn rotations-helper [n a-seq]
  (if (= n 0)
    ()
    (let [new-count (dec n)]
    (cons (seq a-seq) (rotations-helper new-count
                      (concat (rest a-seq) [(first a-seq)]))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
  (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  
  (if (contains? freqs (first a-seq))
    (if (= 0 (count a-seq))
      (if (= nil freqs) {} freqs)
      (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq)))
    (if (= 0 (count a-seq))
      (if (= nil freqs) {} freqs)
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))




(defn my-frequencies [a-seq]
  (my-frequencies-helper nil a-seq))

(defn unf-helper [m-keys m-seq a-map]
  (if (= 0 (count m-keys))
    m-seq
    (concat (repeat (get a-map (first m-keys)) (first m-keys))  (unf-helper (rest m-keys) m-seq a-map)))
)

(defn un-frequencies [a-map]
  (unf-helper (keys a-map) () a-map)
)

 

(defn my-take-helper [n cnt coll]
  (if (or (= cnt n) (= 0 (count coll)))
    []
    (concat [(first coll)] (my-take-helper n (+ 1 cnt) (rest coll))))
)


(defn my-take [n coll]
  (my-take-helper n 0 coll)
)

(defn my-drop [n coll]
  (reverse (my-take-helper (if (>= 0 (- (count coll) n)) 0 (- (count coll) n)) 0 (reverse coll)))
)


(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)] 
)

(defn seq-merge-helper [a b result]
  (cond 
    (and (= 0 (count a)) (= 0 (count b))) result
    (= 0 (count a)) (concat (reverse b) result )
    (= 0 (count b)) (concat (reverse a) result )
    
    :else
    (cond 
    	  (= true (< (first a) (first b))) (seq-merge-helper (rest a) b  (conj result (first a)))	
	      :else (seq-merge-helper a (rest b)  (conj result (first b)))))
)


(defn seq-merge [a-seq b-seq]
  (reverse (seq-merge-helper a-seq b-seq ()))
)

(defn merge-sort [a-seq]
  (let [[left right] (halve a-seq)]
  (if (>= 1 (count a-seq))
    a-seq
    (seq-merge (merge-sort left) (merge-sort right))
  ))
)


(defn print-seq [a-seq]
  (for [x (range 1 (count a-seq)) :when (not= x (count a-seq))] 
       (- (get a-seq x) (get a-seq (- x 1)))))

(defn split-at-multi [nums a-seq]
  (if (empty? nums)
    (if (empty? a-seq) () (cons a-seq ()))
    (cons (take (+ 1 (first nums)) a-seq) (split-at-multi (map (fn [x] (- x (first nums))) (vec (rest nums))) (drop (+ (first nums) 1) a-seq))))
)

(defn split-into-monotonics [a-seq]
  (remove #{()} (split-at-multi (let [a-seq (map (fn [x] (< x 0)) (print-seq a-seq))]
     (filter (complement nil?) (for [x (range 1 (count a-seq)) :when (not= x (count a-seq))] 
       (if (not= (nth a-seq (- x 1) ) (nth a-seq x)) x)
      ))
    ) a-seq) )
)



(defn permutations [things]
  (if (>= 1 (count things))
    (if (= 1 (count things)) (list things) '(()) )
    (for [head things
          tail (permutations (disj (set things) head))]
      (do
        (cons head tail)))))




(defn comb [k l]
  (if (= 1 k) (map vector l)
      (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2))
                    (comb (dec k) (drop (inc %1) l)))
              l))))

(defn powerset [s]
  (conj  (apply concat
         (for [x (range 1 (inc (count s)))]
           (map #(into #{} %) (comb x s))) )#{}))
