(ns recursion)

(defn product [coll]
  (if (empty? coll)
  	1
  	(* (first coll)
  		(product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
  	false
  	(if (empty? (rest coll))
  		true
  		false)))

(defn my-last [coll]
	(if (empty? coll)
		nil
		(if (singleton? coll)
			(first coll)
			(my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
  	nil
  	(if (singleton? a-seq)
  		(first a-seq)
  		(max (first a-seq)
  			 (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
  	seq-1
  	seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
  	nil
  	(if (singleton? a-seq)
  		(first a-seq)
  		(seq-max (first a-seq)
  			 (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
  	a-seq
  	(if (pred? (first a-seq))
  		(cons (first a-seq)
  			  (my-filter pred? (rest a-seq)))
  		(my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
	(cond
		(empty? a-seq) false
		(= elem (first a-seq)) true
		:else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
  	(empty? a-seq)
  		a-seq
  	(pred? (first a-seq))
  		(cons (first a-seq)
  			  (my-take-while pred? (rest a-seq)))
  	:else
  		(my-take-while pred? '())))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
  		a-seq
  		(my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond 
  	(not (== (count a-seq) (count b-seq))) false
  	(and (empty? a-seq) (empty? b-seq)) true
  	(== (first a-seq) (first b-seq))
  		(seq= (rest a-seq) (rest b-seq))
  	:else false))

(defn my-map [f seq-1 seq-2]
  (cond
  	(empty? seq-1) seq-1
  	(empty? seq-2) seq-2
  	:else
  		(cons (f (first seq-1) (first seq-2))
  			  (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
  	1
  	(* n (power n (dec k)))))

(defn fib [n]
  (cond 
  	(zero? n) 0
  	(== n 1) 1
  	:else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat] ;;how-many-times what-to-repeat
  (if (>= 0 how-many-times)
  	'()
  	(cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
  	'()	
  	(cons (dec up-to) (my-range (dec up-to)))))

(defn my-tails-helper [a-seq] ;;OMA LISÄYS
  (if (= (count a-seq) 0)
  	'()
  	(cons a-seq (my-tails-helper (rest a-seq)))))

(defn tails [a-seq]
  (cons '() (my-tails-helper a-seq)))

(defn my-inits-helper [a-seq] ;;OMA LISÄYS
  (if (= (count a-seq) 0)
  	'()
  	(cons (reverse a-seq) (my-inits-helper (rest a-seq)))))

(defn inits [a-seq]
	(let [b-seq (reverse a-seq)]
		(cons '() (my-inits-helper b-seq))))

(defn rot-helper [reps a-seq]
	(if (zero? reps)
		'()
		(cons a-seq (rot-helper (dec reps) (concat (rest a-seq) (vector (first a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
  	'(())
  	(rot-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
	(if (empty? a-seq)
                freqs
                (let [new-freqs (if (contains? freqs (first a-seq))
                						(assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                						(assoc freqs (first a-seq) 1))]
                (my-frequencies-helper new-freqs (rest a-seq)))))
	

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
  	'()
  	(concat 
  		(repeat (get a-map (key (first a-map))) (key (first a-map)))
  		(un-frequencies (apply hash-map (apply concat (rest a-map)))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
  	'()
  	(cons 
  		(first coll)
  		(my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
  	coll
  	(my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [puolet (int (/ (count a-seq) 2))]
  	(vector (my-take puolet a-seq) (my-drop puolet a-seq))))

(defn seq-merge [a-seq b-seq] ;;Pysähtyy, kun toinen listoista loppuu kesken???
  (cond 
  	(and (empty? a-seq) (empty? b-seq)) '()
  	(empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) '()))
  	(empty? a-seq) (cons (first b-seq) (seq-merge '() (rest b-seq)))
  	(< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
  	:else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
  	a-seq
  	(seq-merge 
  		(merge-sort (get (halve a-seq) 0))
  		(merge-sort (get (halve a-seq) 1)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

