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
  (if (empty? a-seq) nil
  (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
	    (first a-seq)
			(let [longer (seq-max (first a-seq) (second a-seq))
			      tail (rest (rest a-seq))]
			     (longest-sequence (conj tail longer)))))

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
	 :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
	(cond 
	 (empty? a-seq) '()
	 (pred? (first a-seq))
	   (conj (my-take-while pred? (rest a-seq)) (first a-seq))
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
  (if (zero? k) 
	 1 
	 (* n (power n (dec k)))))

(defn fib [n]
  (cond 
	 (zero? n) 0
	 (== 1 n) 1
	 :else (+ 
		        (fib (dec n)) 
						(fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
	    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '() (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if 
	 (empty? a-seq)
	 '(())
	 (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (cond
	  (empty? a-seq) '(())
		:else (butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [elem (first a-seq)
	      elemfreq (or (get freqs elem) 0)
	      new-freqs (assoc freqs elem (inc elemfreq))]
	 (if (empty? a-seq)
		   freqs
			 (my-frequencies-helper new-freqs (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
	 (let [elem (first a-map)
	       e-val (val elem)
	       e-key (key elem)]
	 (concat (repeat e-val e-key) (un-frequencies (rest a-map))))))
  ;(apply concat (map repeat (vals a-map) (keys a-map))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n)) '()
	    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond 
	  (empty? coll) '()
	  (zero? n) (seq coll)
	  :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-len (count a-seq)
	      len-1 (int (/ seq-len 2))]
				[(my-take len-1 a-seq) (my-drop len-1 a-seq)]))

(defn seq-merge [a-seq b-seq]
	(cond 
	 (empty? a-seq) b-seq
	 (empty? b-seq) a-seq
   :else (let [
		    a1 (first a-seq)
	      b1 (first b-seq)
				ab-min (min a1 b1)
	      smaller (if (< a1 b1) a-seq b-seq)
	      bigger (if (< a1 b1) b-seq a-seq)]
				(cons ab-min (seq-merge (rest smaller) bigger)))))

(defn merge-sort [a-seq]
  (cond 
	 (empty? a-seq) '()
	 (singleton? a-seq) a-seq
	 :else (let [halvs (halve a-seq)
	       sorted-1 (merge-sort (first halvs))
	       sorted-2 (merge-sort (second halvs))]
       (seq-merge sorted-1 sorted-2))))

;	 (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
  (let [monotonic? (fn [a-seq] (or (apply < a-seq) (apply > a-seq)))
	      first-monos (take-while monotonic? (rest (inits a-seq)))
	      mono-count (count first-monos)
				longest-mono (last first-monos)] 
				(cons longest-mono (split-into-monotonics (drop mono-count a-seq))))))

(defn permutations [a-set]
  (cond 
	 (not (set? a-set)) (permutations (set a-set))
	 (empty? a-set) '(())
	 (= (count a-set) 1) (seq [(seq a-set)])
	 (= (count a-set) 2) 
	   (let [a-seq (seq a-set)
		       b-seq (reverse a-seq)
		       perms (seq [a-seq b-seq])]
			perms)
   :else
	   (apply concat (map 
		   (fn [elem]
				 (let [perms (permutations (disj a-set elem))
				       full-perms (map (fn [perm] (cons elem perm)) perms)]
				  full-perms))
			  a-set))))

	 

(defn powerset [a-set]
  (cond
    (not (set? a-set)) (powerset (set a-set))
    (empty? a-set) #{#{}}
    :else 
      (set (conj 
        (apply concat
        (map powerset
          (map (fn [elem] (disj a-set elem)) a-set)))
        a-set))))

