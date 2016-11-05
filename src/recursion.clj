(ns recursion)

(defn product [coll]
	(if (empty? coll)
		1
		(* (first coll)
			(product (rest coll))))
)
; (* 1 (product (cons 2 (cons 4))))
; (* 1 (* 2 (product (cons 4))))
; (* 1 (* 2 (4)))
; (* 1 (8))
; (8)

(defn singleton? [coll]
	(and (empty? (rest coll)) (not (empty? coll)))
)

(defn my-last [coll]
	(if (empty? coll)
		nil
		(if (singleton? coll)
			(first coll)
			(my-last (rest coll))
		)
	)
)

(defn max-element [a-seq]
	(if (empty? a-seq)
		nil
		(if (singleton? a-seq)
			(first a-seq)
			(max (first a-seq) (max-element (rest a-seq)))
		)
	)
)

(defn seq-max [seq-1 seq-2]
	(if (> (count seq-1) (count seq-2))
		seq-1
		seq-2
	)
)

(defn longest-sequence [a-seq]
	(if (empty? a-seq)
		nil
		(if (singleton? a-seq)
			(first a-seq)
			(seq-max (first a-seq) (longest-sequence (rest a-seq)))
		)
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
		(empty? a-seq)
			false
		(singleton? a-seq)
			(= elem (first a-seq))
		(= elem (first a-seq))
			true
		:else
			(sequence-contains? elem (rest a-seq))
	)
)

(defn my-take-while [pred? a-seq]
	(cond
		(empty? a-seq)
			'()
		(pred? (first a-seq))
			(cons (first a-seq) (my-take-while pred? (rest a-seq)))
		:else
			'()
	)
)

(defn my-drop-while [pred? a-seq]
	(cond
		(empty? a-seq)
			'()
		(pred? (first a-seq))
			(my-drop-while pred? (rest a-seq))
		:else
			a-seq
	)
)

(defn seq= [a-seq b-seq]
	(cond
		(and (empty? a-seq) (empty? b-seq))
			true
		(or (empty? a-seq) (empty? b-seq))
			false
		(= (first a-seq) (first b-seq))
			(seq= (rest a-seq) (rest b-seq))
		:else
			false
	)
)

(defn my-map [f seq-1 seq-2]
	(if (and (not (empty? seq-1)) (not (empty? seq-2)))
		(cons
			(f (first seq-1) (first seq-2))
			(my-map f (rest seq-1) (rest seq-2))
		)
		'()
	)
)

(defn power [n k]
	(if (zero? k)
		1
		(* n (power n (- k 1)))
	)
)

(defn fib [n]
	(cond
		(= n 0)
			0
		(= n 1)
			1
		:else
			(+ (fib (- n 1)) (fib (- n 2)))
	)
)

(defn my-repeat [how-many-times what-to-repeat]
	(cond
		(> how-many-times 0)
			(cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
		:else
			'()
	)
)

(defn my-range [up-to]
	(cond
		(= up-to 0)
			'()
		:else
			(conj (my-range (- up-to 1)) (- up-to 1))
	)
)

(defn tails [a-seq]
	(if (empty? a-seq)
		['()]
		(cons (lazy-seq a-seq) (tails (rest a-seq)))
	)
)

(defn inits [a-seq]
	(if (empty? a-seq)
		['()]
		(cons (lazy-seq a-seq) (inits (butlast a-seq)))
	)
)

(defn rotations [a-seq]
	(if (empty? a-seq)
		(cons '() '())
		(rest (map concat (reverse (tails a-seq)) (inits a-seq)))
	)
)

(defn my-frequencies-helper [freqs a-seq]
	(if (= 0 (count a-seq))
		freqs
		(let [x (get freqs (first a-seq))]
			(my-frequencies-helper 
				(if (nil? x)
					(assoc freqs (first a-seq) 1)
					(assoc freqs (first a-seq) (+ x 1)))
				(rest a-seq))
		)
	)
)

(defn my-frequencies [a-seq]
	(my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
	(apply concat (map (fn [x] (repeat (get a-map x) x)) (keys a-map)))
)

(defn my-take [n coll]
	(cond
		(= n 0)
			'()
		(empty? coll)
			'()
		:else
			(cons (first coll) (my-take (- n 1) (rest coll)))
	)
)

(defn my-drop [n coll]
	(if (= n 0)
		coll
		(my-drop (- n 1) (rest coll))
	)
)

(defn halve [a-seq]
	(let [c (count a-seq)
		f (int (/ c 2))]
		[(my-take f a-seq) (my-drop f a-seq)]
	)
)

(defn seq-merge [a-seq b-seq]
	(cond
		(and (empty? a-seq) (empty? b-seq))
			'()
		(empty? a-seq)
			b-seq
		(empty? b-seq)
			a-seq
		(< (first a-seq) (first b-seq))
			(conj (seq-merge (rest a-seq) b-seq) (first a-seq))
		:else
			(conj (seq-merge a-seq (rest b-seq)) (first b-seq))
	)
)

(defn merge-sort [a-seq]
	(cond
		(< (count a-seq) 2)
			a-seq
		:else
			(let [[a b] (halve (sort a-seq))]
				(seq-merge (merge-sort a) (merge-sort b))
			)
	)
)
(defn split-helper [a-seq n f]
	(cond
		(empty? a-seq)
			'()
		:else
			(cons (take (if f (+ n 1) n) a-seq) (split-helper (drop (if f (+ n 1) n) a-seq) n false))
	)
)

(defn split-into-monotonics [a-seq]
	(let [x (int (/ (count a-seq) 2))
		d (= x (/ (count a-seq) 2))]
		(split-helper a-seq 2 (not d))
	)
)

(defn permutations [a-set]
	(lazy-seq
		(if
			(seq (rest a-set))
			(apply concat
				(for [x a-set]
					(map #(cons x %) (permutations (remove #{x} a-set)))))
			[a-set]
		)
	)
)

(defn powerset [a-set]
	(if (empty? a-set) 
		'(#{})
		(clojure.set/union 
			(powerset (next a-set))
			(map #(conj % (first a-set)) (powerset (next a-set)))
		)
	)
)

