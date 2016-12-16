(ns recursion)

(defn product [coll]
  (if (empty? coll) 
    1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]  
  (if (empty? coll)
    false
    (do 
      (if (empty? (rest coll))
        true
        false ))))


(defn my-last [coll]
  (if (empty? coll)
    nil
    (do (if (singleton? coll)
          (first coll)
          (my-last (rest coll))))))

(defn max-helper [a-seq curr-max]
  (if (singleton? a-seq)
    (max curr-max (first a-seq))
    (do
      (if (> (first a-seq) curr-max)
        (max-helper (rest a-seq) (first a-seq))
        (max-helper (rest a-seq) curr-max)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max-helper a-seq (first a-seq))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-seq-helper [a-seq longest]
  (if (singleton? a-seq)
    (seq-max (first a-seq) longest)
    (do
      (if (not (seq-max (first a-seq) longest))
        (longest-seq-helper (rest a-seq) longest)
        (longest-seq-helper (rest a-seq) (first a-seq))))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (longest-seq-helper a-seq (first a-seq))))

(defn my-map [f a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (f (first a-seq))
          (my-map f (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else
    (sequence-contains? elem (rest a-seq))))
    
(defn my-take-while-helper [pred? a-seq coll]
  (let [a-first (first a-seq) ]
    (cond
      (empty? a-seq) coll
      (pred? a-first) (my-take-while-helper pred? 
                                            (rest a-seq) 
                                            (conj coll a-first))
    :else  coll )))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (my-take-while-helper pred? a-seq [])))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) 
      (my-drop-while pred? (rest a-seq))
      a-seq)))


(defn seq= [a-seq b-seq]
  (cond 
    (not (= (count a-seq) (count b-seq))) false
    (and (empty? a-seq) (empty? a-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false ))

            
(defn my-map [f seq-1 seq-2]
  (cond 
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (if ( < n 2) 
  n
  (+ (fib (dec n)) (fib(dec(dec n))))))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 0) ()
    (= how-many-times 1) (list what-to-repeat)
    :else 
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [ num (dec up-to) ]
    (cond 
      (< num 0) ()
      (= num 0) (list num)
      :else (cons num (my-range num)))))

;(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
;(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn tails [a-seq]
  (let [ my-list (reverse (reverse a-seq))]
  (if (empty? my-list)
    (list ())
    (conj (tails(rest a-seq)) my-list ))))

(defn inits [a-seq]
  (let [ tmp-list (tails (reverse a-seq)) ]
    (map reverse tmp-list)))
    
; (rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))

(defn rotate [a-seq]  
  (rest (conj a-seq (first a-seq))))


(defn left-rotate [a-seq size]
  (let [ tmp-seq (rotate a-seq) ]
    (if (= size 0) 
      ()
      (conj (left-rotate (vec tmp-seq) (dec size)) a-seq ))))

(defn rotations [a-seq]
  (if (= (count a-seq )0)
    (list ())
    (left-rotate a-seq (count a-seq))))

; (my-frequencies [:a "moi" :a "moi" "moi" :a 1]) 
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [ elem (first a-seq) ] 
      (if (contains? freqs elem)
        (my-frequencies-helper (assoc freqs elem (inc (get freqs elem))) (rest a-seq))
        (my-frequencies-helper (assoc freqs elem 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (do 
      (let [ elem (first a-map) ]
        (un-frequencies-helper (concat a-seq (repeat (nth elem 1) (first elem))) (rest a-map))))))

; (un-frequencies {:a 3 :b 2 "^_^" 1})  
(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (un-frequencies-helper [] a-map)))

(defn my-take [n coll]
  (if (empty? coll)
    ()
    (do 
      (let [ num (if (< n (count coll))
                   n
                   (count coll)) ]
        (if ( = n 0) 
          ()
          (conj (my-take (dec n) (rest coll)) (first coll)))))))


(defn my-drop [n coll]
  (let [ num (if (> n (count coll))
               (count coll)
               n) ]
    (if (= num 0)
      coll
      (my-drop (dec num) (rest coll)))))

(defn halve [a-seq]
  (let [ dividor (int (/ (count a-seq) 2)) ]
    [(my-take dividor a-seq) (my-drop dividor a-seq)]))

(defn seq-merge-helper [ a-seq b-seq sorted ]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) (reverse sorted)
    (empty? a-seq) (seq-merge-helper () (rest b-seq)  (conj sorted (first b-seq)))
    (empty? b-seq) (seq-merge-helper (rest a-seq) ()  (conj sorted (first a-seq)))
    :else
    (if (< (first a-seq) (first b-seq))
      (seq-merge-helper (rest a-seq) b-seq  (conj sorted (first a-seq)))
      (seq-merge-helper a-seq (rest b-seq)  (conj sorted (first b-seq))))))
        
  
(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq ()))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= (count a-seq) 1))
    a-seq
    (do
      (let [ [ left right ] (halve a-seq) ]
        (seq-merge (merge-sort left) (merge-sort right))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

