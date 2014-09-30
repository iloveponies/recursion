(ns recursion
  (:use [clojure.set]))

(defn product [coll]
  (if (empty? coll)
	1
	(* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (boolean (and (seq coll) (empty? (rest coll)))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
	(first coll)
	(my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
	(first a-seq)
	(max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
	seq-1
	seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
	nil
	(seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) 			()
   (pred? (first a-seq)) 	(cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else 					(my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) 			false
   (= elem (first a-seq)) 	true
   :else 					(sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) ((complement pred?) (first a-seq)))
	()
	(cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) 						()
   ((complement pred?) (first a-seq)) 	a-seq
   :else								(my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (empty? a-seq) 						(empty? b-seq)
   (empty? b-seq) 						(empty? a-seq)
   (= (first a-seq) (first b-seq)) 		(seq= (rest a-seq) (rest b-seq))
   :else 								false))

(seq= [1 2 3] [1 2])

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
	()
	(cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
	1
	(* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0) (= n 1))
	n
	(+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
	()
	(cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
	()
	(cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
	'(())
	(cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper [a-seq counter]
  (let [rotated-seq (concat (rest a-seq) [(first a-seq)])]
	(if (= counter 1)
	  (seq [a-seq])
	  (cons a-seq (rotations-helper rotated-seq (dec counter))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
	'(())
	(rotations-helper (seq a-seq) (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
	freqs
	(if (contains? freqs (first a-seq))
	  (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
	  (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
	()
	(concat
	 (repeat (get a-map (first (first a-map))) (first (first a-map)))
	 (un-frequencies (into {} (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
	()
	(cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
	coll
	(my-drop (dec n)(rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
	(vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge-helper [a b-seq]
  (if (or (empty? b-seq) (<= a (first b-seq)))
	(cons a b-seq)
	(cons (first b-seq) (seq-merge-helper a (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
	b-seq
	(seq-merge (rest a-seq) (seq-merge-helper (first a-seq) b-seq))))

(defn merge-sort [a-seq]
  (let [halves (halve a-seq)]
	(if (or (empty? a-seq) (empty? (rest a-seq)))
	  a-seq
	  (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn split-into-monotonics [a-seq]
  (let [monotonic
		(last (take-while #(or (apply <= %) (apply >= %)) (rest (inits a-seq))))]
	(if (empty? a-seq)
	  ()
	  (cons monotonic (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations-helper [fixed not-fixed counter]
  (let [split 			(split-at counter not-fixed)
		elem 			(first (second split))
		new-fixed 		(concat fixed [elem])
		new-not-fixed 	(concat (first split) (rest (second split)))]
	(cond
	 (empty? new-not-fixed)
	 	(seq [new-fixed])
	 (= counter 0)
	 	(permutations-helper new-fixed new-not-fixed (dec (count new-not-fixed)))
	 :else
	 	(concat
		 (permutations-helper new-fixed new-not-fixed (dec (count new-not-fixed)))
		 (permutations-helper fixed not-fixed (dec counter))))))

(defn permutations [a-set]
  (if (empty? a-set)
	'(())
	(permutations-helper '() a-set (dec (count a-set)))))

(defn binary-vector [number length]
  (cond
   (= length 0) []
   (= number 0) (conj (binary-vector number (dec length)) 0)
   :else 		(conj (binary-vector (quot number 2) (dec length)) (mod number 2))))

(defn addressed-set [a-set addressing-vector]
  (cond
   (empty? a-set)
   	#{}
   (= (first addressing-vector) 1)
   	(union #{(first a-set)} (addressed-set (rest a-set) (rest addressing-vector)))
   :else
   	(addressed-set (rest a-set) (rest addressing-vector))))

(defn powerset-helper [a-set addressing-counter maximum length]
  (if (= addressing-counter maximum)
	#{(addressed-set a-set (binary-vector addressing-counter length))}
   	(union
	 #{(addressed-set a-set (binary-vector addressing-counter length))}
	 (powerset-helper a-set (inc addressing-counter) maximum length))))

(defn powerset [a-set]
  (powerset-helper a-set 0 (power 2 (count a-set)) (count a-set)))

