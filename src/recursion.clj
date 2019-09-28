(ns recursion)

(defn product [coll]
  (if (empty? coll)
       1
	  (* (first coll) (product (rest coll))) ) )

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))) )

(defn my-last [coll]
  (cond
	(empty? coll) nil
	(singleton? coll) (first coll)
	:else (my-last (rest coll)) ) )
		
(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
		:else (max (first a-seq)
		            (max-element (rest a-seq)) ) ) )
															
(defn seq-max [seq-1 seq-2]
  (letfn [(is-longer? [seq-1 seq-2]
			(cond
				(empty? seq-1) false
				(empty? seq-2) true
				:else (is-longer? (rest seq-1) (rest seq-2)) ) ) ]
  (if (is-longer? seq-1 seq-2)
      seq-1
	  seq-2 ) ) )

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
	(singleton? a-seq) (first a-seq)
	:else (seq-max (first a-seq) (longest-sequence (rest a-seq))) ) )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
	  '()
	  (let [first-elem (first a-seq)]
	    (if (pred? first-elem)
		    (cons first-elem
			      (my-filter pred? (rest a-seq)) )
			(my-filter pred? (rest a-seq)) ) ) ) )

(defn my-equal? [elem1 elem2]
  "tests equality for both numbers and atoms"
  (if (number? elem1)
	  (if (number? elem2)
	      (== elem1 elem2)
		  false )
	  (= elem1 elem2) ) )

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
	    false
    (my-equal? elem (first a-seq))
	    true
	:else
	   (sequence-contains? elem (rest a-seq)) ) )

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    ((complement pred?) (first a-seq)) '()
	:else (cons (first a-seq)
		        (my-take-while pred? (rest a-seq)) ) ) )

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq)
		  ((complement pred?) (first a-seq)) )
	   a-seq
	  (my-drop-while pred? (rest a-seq)) ) )
	  
(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or  (empty? a-seq) (empty? b-seq)) false
   (my-equal? (first a-seq) (first b-seq))
	    (seq= (rest a-seq) (rest b-seq))
    :else false ) )
	

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
		  (empty? seq-2) )
	   '()
	  (cons
	    (f (first seq-1) (first seq-2))
		(my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
	   1
	  (* n (power n (dec k))) ) )

(defn fib [n]
  (cond
   (>= n 2)  (+ (fib (- n 1)) (fib (- n 2)))
   (== n 1)  1
   (zero? n) 0 ) )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      '()
	  (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)) ) )

(defn my-range [up-to]
  (if (zero? up-to)
      '()
	  (let [next (dec up-to)]
	    (cons next (my-range next)) ) ) )
      

(defn tails [a-seq]
  (if (empty? a-seq)
       '(())
	  (cons a-seq (tails (rest a-seq))) ) )
	  
(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))) )

(defn rotations [a-seq]
  (letfn [(h [head-seq tail-vec]
			(if (empty? head-seq)
			    '()
				(cons (concat head-seq tail-vec)
				      (h (rest head-seq) (conj tail-vec (first head-seq))) ) ) )]
	(if (empty? a-seq)
		'(())
		(h a-seq []) ) ) )
		
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
       freqs
	   (let [elem (first a-seq)]
        (my-frequencies-helper
	        (assoc freqs elem
	          (if (contains? freqs elem)
			      (inc (get freqs elem))
				  1 ) )
			(rest a-seq) ) ) ) )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq) )
  
(defn un-frequencies [a-map]
  (if (empty? a-map)
     '()
	 (let [head (first a-map)]
	  (concat (repeat (second head) (first head))
	          (un-frequencies (rest a-map)) ) ) ) )

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
       '()
	  (cons (first coll)
	        (my-take (dec n) (rest coll)) ) ) )

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0))
       coll
	  (my-drop (dec n) (rest coll)) ) )

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector (my-take mid a-seq) (my-drop mid a-seq)) ) ) 
	
(defn seq-merge
  ([a-seq b-seq] (seq-merge < a-seq b-seq))
  ([compare a-seq b-seq] 
    (cond
      (empty? a-seq) b-seq
	  (empty? b-seq) a-seq
	  (compare (first a-seq) (first b-seq))
	      (cons (first a-seq)
	            (seq-merge compare (rest a-seq) b-seq) )
	  :else
	      (cons (first b-seq)
	            (seq-merge compare a-seq (rest b-seq)) ) ) ) )
				
(defn merge-sort
  "Sorts a given sequence in the order defined by compare - a comparing function.
   Works for sequences of any types, assuming a sensible comparing function is used.
   Uses < as the default comparing function."
  ([a-seq] (merge-sort < a-seq))
  ([compare a-seq]
    (if (< (count a-seq) 2)
         a-seq
	    (let [halve-seqs (halve a-seq)]
	      (seq-merge compare (merge-sort (first halve-seqs))
						  (merge-sort (second halve-seqs)) ) ) ) ) )

; My split-into-monotonics uses an idea of a combiner-function borrowed from
; the book The Little Schemer.
; the function split-into-monotonics itself (through the helper function h)
; keeps track of past runs of monotonic subsequences  inside a-seq and combines
; these runs with the help of the function combine to create monotonic splits.
; It uses a helper function h, which is defined as
; h: ((seq -> seq) * seq * boolean * integer * seq) -> seq
;
; combine is a function used to combine individual runs of monotonic subsequences in
; a-seq into a single monotonic split-into-monotonics.
;
; run holds the current monotonic run.
;
; compare is the comparison function used for the current monotonic run
;
; prev-val is the latest value of the current monotonic run; it's being compared
; against the next a-seq value not yet examined.
;
; I remembered a trick I learned from The Little Schemer a long time ago, and
; decided to see if I still remember it.

(defn split-into-monotonics [a-seq]
  (letfn [(h [combine run compare prev-val a-seq]
		    (if (empty? a-seq)														; are we done yet?
				(combine (reverse run))												; if so, combine the last run with all the others
				(let [next-val (first a-seq)]
				  (cond
				    (not compare)													; do we have to start a new run?
					  (h combine (cons next-val run)  								; if so, start a new run
					    (cond						  								; in that case we have to calculate the new compare function
						  (< prev-val next-val) <=    								; if the run is monotonically descending, choose <=
						  (> prev-val next-val) >=   								; if the run is monotonically ascending, choose >=
						  :else nil )				 								; else delay the decision until are certain about the correct comparator
						next-val
						(rest a-seq) )
					(compare prev-val next-val)		  								; does the next value satisfy the comparison criteria of the current run?
					  (h combine (cons next-val run) compare next-val (rest a-seq)) ; then we simply continue our current run
					:else							  								; else the current run has ended, prepare for the next run
				      (h (fn [rest] (concat (combine (reverse run)) (list rest)))	; adjust the combiner accordingly
					     (list next-val) nil next-val (rest a-seq) ) ) ) ) )]
	(h list (list (first a-seq)) nil (first a-seq) (rest a-seq)) ) )

(defn permutations [a-set]
  (if (empty? a-set)
	 '(())
      (mapcat (fn [elem] (map (fn [perm] (cons elem perm)) (permutations (remove (fn [i] (= i elem)) a-set)))) a-set) ) )
	    

; concerning the next problem,
; the following code returned false for me when running "lein midje"-command:
; (defn powerset [a-set]
;    (set? a-set) )
;
; It took me an hour to figure out the problem in my original code and then it turned out
; that not all the tests give a set as an input to the function powerset.
; This problem made me extremely confused... Are we supposed to work on sets here or not?
; thus I had to make sure every "set" is indeed a set by explicitly changing them into a set
; with the aid of (into #{} ...)
;
; Anyway, the way Clojure handles all the different sequences seems extremely confusing.
; I really miss Common LISP, or even the original MIT Scheme.

(defn powerset [a-set]
  (if (empty? a-set)
	   #{#{}}
	   (let [elem (first a-set)
			 pw (powerset (disj (into #{} a-set) elem))]
	     (clojure.set/union pw (into #{} (map (fn [a-set] (conj a-set elem)) pw))) ) ) )