(ns recursion)

(defn product [coll]
	(if (empty? coll)
		1
		(* (first coll) (product (rest coll)))
	)
)

(defn singleton? [coll]
	(and (not (empty? coll)) (empty? (rest coll)))
)

(defn my-last [coll]
	(if (or (singleton? coll) (empty? coll))
		(first coll)
		(my-last (rest coll))
	)
)

(defn max-element [a-seq]
	(if (or (singleton? a-seq) (empty? a-seq))
		(first a-seq)
		(max (my-last a-seq) (max-element (rest a-seq)))
	)
)

(defn seq-max [seq-1 seq-2]
	(if (> (count seq-1) (count seq-2))
		seq-1
		seq-2
	)
)

(defn longest-sequence [a-seq]
	(if (or (singleton? a-seq) (empty? a-seq))
		(first a-seq)
		(seq-max (my-last a-seq) (longest-sequence (rest a-seq)))
	)
)

(defn my-filter [pred? a-seq]
	(if (empty? a-seq)
		a-seq
		(if (pred? (first a-seq))
			(cons (first a-seq) (my-filter pred? (rest a-seq)))
			(my-filter pred? (rest a-seq))
		)	
	)
)

(defn sequence-contains? [elem a-seq]
	(cond
		(empty? a-seq) false 
		(= (first a-seq) elem) true
		:else (sequence-contains? elem (rest a-seq))
	)
)

(defn my-take-while [pred? a-seq]
	(cond
		(empty? a-seq) (empty a-seq)
		(pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
		:else (empty a-seq)
	)
)

(defn my-drop-while [pred? a-seq]
	(cond
		(empty? a-seq) (empty a-seq)
		(pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
		:else a-seq
	)
)

(defn seq= [a-seq b-seq]
	(cond
		(and (empty? a-seq) (empty? b-seq)) true
		(not (= (first a-seq) (first b-seq))) false
		(or (empty? a-seq) (empty? b-seq)) false
		:else (seq= (rest a-seq) (rest b-seq))
	)
)

(defn my-map [f seq-1 seq-2]
	(cond 
		(empty? seq-2) (empty seq-2)
		:else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
	)
)

(defn power [n k]
	(cond
		(= k 0) 1
		(= k 1) n
		:else (* n (power n (- k 1)))
	)
)

(defn fib [n]
	(cond
		(< n 2) n
		:else (+ (fib (- n 2)) (fib (- n 1)))
	)
)

(defn my-repeat [how-many-times what-to-repeat]
	(cond 
		(> how-many-times 0) (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
		:else (empty (seq '(1)))
	)
)

(defn my-range [up-to]
	(cond
		(> up-to 0) (cons (- up-to 1) (my-range (- up-to 1)))
		:else (empty (seq '(1)))
	)
)

(defn tails [a-seq]
	(cond
		(not (empty? a-seq))(cons a-seq (tails (rest a-seq)))
		:else [a-seq]
	)
)

(defn inits [a-seq]
	(cond
		(not (empty? a-seq))(cons a-seq (inits (drop-last a-seq)))
		:else [a-seq]
	)
)

(defn rotations [a-seq]
	(cond
		(not (empty? a-seq)) (map (fn [x] (concat (drop x a-seq) (take x a-seq) )) (range (count a-seq)))
		:else [a-seq]
	)
)

(defn my-frequencies-helper [freqs a-seq]
	(cond
		(not (empty? a-seq)) (let [x (first a-seq)] 
			(cond
				(contains? freqs x) (my-frequencies-helper (assoc freqs x (inc (get freqs x))) (rest a-seq))
				:else (my-frequencies-helper (assoc freqs x 1) (rest a-seq))
			)
		)
		:else freqs
	)
)

(defn my-frequencies [a-seq]
	(my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
	(apply concat (map #(my-repeat (get a-map %) %) (keys a-map)))
)

(defn my-take [n coll]
	(cond
		(or (== n 0) (empty? coll)) (empty coll) 
		:else (cons (first coll) (my-take (- n 1) (rest coll)))
	)
)

(defn my-drop [n coll]
	(cond
		(or (== n 0) (empty? coll)) coll
		:else (my-drop (- n 1) (rest coll))
	)
)

(defn halve [a-seq]
	(list (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq))
)

(defn seq-merge [a-seq b-seq]
	(cond
		(empty? a-seq) b-seq
		(empty? b-seq) a-seq
		(> (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
		:else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
	)
)

(defn merge-sort [a-seq]
	(cond
		(< (count a-seq) 2) a-seq
		:else (let [[left right] (halve a-seq)]
			(seq-merge (merge-sort left) (merge-sort right))
		)
	)
)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

