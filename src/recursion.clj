(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
	(* (first coll)
	   (product (rest coll)))))
	   
;(product [1 2 4])
;= (product '(1 2 4))
;= (product (cons 1 (cons 2 (cons 4 '()))))
;=> (* 1 (product (cons 2 (cons 4 '()))))
;=> (* 1 (* 2 (product (cons 4 '()))))
;=> (* 1 (* 2 (* 4 (product '()))))
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll) nil (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
	nil
	(max (first a-seq) (if (empty? (rest a-seq)) 0 (max-element (rest a-seq))))))

;I guess this should be done without 'count' but with just two parameters it's hard to pass the original state.
(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
	(seq-max (first a-seq) (if (empty? (rest a-seq)) [] (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
	(if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
	  false
	(== (first a-seq) elem)
	  true
	:else
	  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
	  '()
	(pred? (first a-seq))
	  (cons (first a-seq) (my-take-while pred? (rest a-seq)))
	:else
	  '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
	  '()
	(pred? (first a-seq))
	  (my-drop-while pred? (rest a-seq))
	:else
	  a-seq))
	

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
	  true
	(= (first a-seq) (first b-seq))
	  (seq= (rest a-seq) (rest b-seq))
	:else
	  false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
	'()
	(cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

;This doesn't work for 0^0 which should of course be undefined.
(defn power [n k]
  (if (== k 0)
    1
	(* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (== n 0) 0
	(== n 1) 1
	:else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
	(cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
	(cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    #{'()}
	(conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [n a-seq]
  (let [next (cons (first (reverse a-seq)) (reverse (rest (reverse a-seq))))]
    (if (< n (count a-seq))
	  (conj (rotations-helper (inc n) next) next)
	  '())))

;This won't work properly if the original sequence has a non-trivial cycle.
(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
	(seq (rotations-helper 0 a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [current (first a-seq)]
    (cond
	  (empty? a-seq) freqs
      (contains? freqs current) (my-frequencies-helper (assoc freqs current (inc (get freqs current))) (rest a-seq))
      :else (my-frequencies-helper (assoc freqs current 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (un-frequencies (rest a-map)) (my-repeat (first (vals a-map)) (first (keys a-map))))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
	(conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (or (== n 0) (empty? coll))
    coll
	(my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (conj [] (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
	(empty? b-seq) a-seq
	(<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
	:else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (seq-merge (merge-sort (get (halve a-seq) 0)) (merge-sort (get (halve a-seq) 1)))
	a-seq))

(defn increasing? [a-seq]
  (cond
    (empty? a-seq) true
	(singleton? a-seq) true
	(<= (first a-seq) (first (rest a-seq))) (increasing? (rest a-seq))
	:else false))
	
(defn decreasing? [a-seq]
  (cond
    (empty? a-seq) true
	(singleton? a-seq) true
	(>= (first a-seq) (first (rest a-seq))) (decreasing? (rest a-seq))
	:else false))
	
(defn take-while-increasing [pred-seq a-seq]
  (cond
    (empty? a-seq)
	  (reverse pred-seq)
	(decreasing? (conj pred-seq (first a-seq)))
	  (take-while-increasing (conj pred-seq (first a-seq)) (rest a-seq))
	:else
	  (reverse pred-seq)))

(defn take-while-decreasing [pred-seq a-seq]
  (cond
    (empty? a-seq)
	  (reverse pred-seq)
	(increasing? (conj pred-seq (first a-seq)))
	  (take-while-decreasing (conj pred-seq (first a-seq)) (rest a-seq))
	:else
	  (reverse pred-seq)))
	  
(defn split-into-monotonics-helper [a-seq]
  (cond
    (empty? a-seq) []
	(singleton? a-seq) (conj [] a-seq)
	(< (first a-seq) (first (rest a-seq))) (let [taken (take-while-increasing '() a-seq)]
	                                            (conj (split-into-monotonics-helper (drop (count taken) a-seq)) taken))
	(> (first a-seq) (first (rest a-seq))) (let [taken (take-while-decreasing '() a-seq)]
	                                            (conj (split-into-monotonics-helper (drop (count taken) a-seq)) taken))
	:else (conj (cons (first a-seq) (first (split-into-monotonics-helper (rest a-seq)))) (rest (split-into-monotonics-helper (rest a-seq))))))
	
(defn split-into-monotonics [a-seq]
  (seq (reverse (split-into-monotonics-helper a-seq))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

