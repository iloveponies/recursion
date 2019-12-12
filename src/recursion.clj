(ns recursion)

(defn product [coll]
	(if (empty? coll)
		1
		(* (first coll) (product (rest coll)))))

(defn singleton? [coll]
	(and (not (empty? coll))
		 (empty? (rest coll))))

(defn my-last [coll]
	(cond
		(empty? coll) nil
		(singleton? coll) (first coll)
		:else (my-last (rest coll))))


(defn max-element [a-seq]
	(let [a (first a-seq)]
		(cond
			(empty? a-seq) nil
			(singleton? a-seq) a
			:else (max a (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
	(if (> (count seq-1) (count seq-2))
		seq-1
		seq-2))

(defn longest-sequence [a-seq]
	(let [a (first a-seq)]
		(cond
			(empty? a-seq) nil
			(singleton? a-seq) a
			:else (seq-max a (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
	(if (empty? a-seq)
		a-seq
		(let [a (first a-seq)
			  r (my-filter pred? (rest a-seq))]
			(if (pred? a)
				(cons a r)
				r))))

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
		(or (empty? a-seq) (empty? b-seq)) false
		(= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
		:else false))

(defn my-map [f seq-1 seq-2]
	(if (or (empty? seq-1) (empty? seq-2))
		'()
		(cons
			(f (first seq-1) (first seq-2))
			(my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
	(if (== k 0)
		1
		(* n (power n (dec k)))))

(defn fib [n]
	(if (< n 2)
		n
		(+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
	(if (<= how-many-times 0)
		'()
		(cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
	(if (>= 0 up-to)
		'()
		(let [next-number (dec up-to)]
			(cons next-number (my-range next-number)))))

(defn tails [a-seq]
	(if (empty? a-seq)
		'(())
		(conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
	(reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
	(if (empty? a-seq)
		'(())
		(for [i (range (count a-seq))]
			(concat (drop i a-seq) (take i a-seq))
			)))

(defn my-frequencies-helper [freqs a-seq]
	(if (empty? a-seq)
		freqs
		(let [new-freqs (update-in freqs [(first a-seq)] (fnil inc 0))]
			(my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
	(my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
	(reduce concat (map #(repeat (second %) (first %)) a-map)))

(defn my-take [n coll]
	(if (or (>= 0 n) (empty? coll))
		'()
		(cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
	(if (or (>= 0 n) (empty? coll))
		coll
		(my-drop (dec n) (rest coll))))

(defn halve [a-seq]
	(let [h (/ (dec (count a-seq)) 2)]
		(vector (my-take h a-seq) (my-drop h a-seq))))

(defn seq-merge [a-seq b-seq]
	(cond
		(empty? a-seq) b-seq
		(empty? b-seq) a-seq
		:else
		(let [a (first a-seq) b (first b-seq)]
			(if (< a b)
				(cons a (seq-merge (rest a-seq) b-seq))
				(cons b (seq-merge (rest b-seq) a-seq))))))

(defn merge-sort [a-seq]
	(if (>= 1 (count a-seq))
		a-seq
		(apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
	(let [monotonic? (fn mono-req[x]
						 (if (>= 2 (count x))
							 true
							 (let [[a b c] x]
								 (if (< 0 (* (- a b) (- b c)))
									 (mono-req (rest x))
									 false
									 ))))
		  start (last (take-while monotonic? (inits a-seq)))]
		(if (empty? a-seq)
			'()
			(cons start (split-into-monotonics (drop (count start) a-seq))))))

(defn permutations [a-set]
	(cond
		(empty? a-set) '(())
		(= 1 (count a-set)) (vector a-set)
		:else (let [join (fn [x y] (map #(cons x %) y))]
				  (->> a-set
					   (rotations)
					   (map #(join (first %) (permutations (rest %))))
					   (apply concat)
					   ))))


(defn powerset [a-set]
	(if (empty? a-set)
		#{#{}}
		(let [actually-a-set (set a-set)
			  a (first actually-a-set)
			  s (powerset (disj actually-a-set a))]
			(clojure.set/union s (map #(conj % a) s)))))

