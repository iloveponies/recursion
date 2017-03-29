(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
	  (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
	seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
	(if (singleton? a-seq)
      (first a-seq)
	  (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
	(if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
	  (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
	(if (== elem (first a-seq))
      true
	  (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
	(if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
	  '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
	(if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
	  (seq a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
	(not (= (empty? a-seq) (empty? b-seq))) false
	(== (first a-seq) (first b-seq)) 
	  (seq= (rest a-seq) (rest b-seq))
	:else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
	(cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
	(* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
	(== n 1) 1
	:else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
	(cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
	(cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
	(cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
	(rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [fst (first a-seq)
       freq (freqs fst)
	   remain (rest a-seq)]
    (if (empty? a-seq)
	  freqs
	  (if (number? freq)
	    (my-frequencies-helper (assoc freqs fst (inc freq)) remain)
		(my-frequencies-helper (assoc freqs fst 1) remain)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [keys (keys a-map)
		vals (vals a-map)]
    (apply concat (map (fn[n k] (repeat k n)) keys vals))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
	(cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
	(if (pos? n)
	  (my-drop (dec n) (rest coll))
	  (cons (first coll) (my-drop n (rest coll))))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
	(empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
	(>= (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [[a b] (halve a-seq)]
    (cond
    (empty? a-seq) '()
	(== 1 (count a-seq)) (seq a-seq)
	:else (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
	(let [monotonic? (fn [seq] (or (apply <= seq) (apply >= seq)))
	      ms (first (reverse (take-while monotonic? (rest (inits a-seq)))))]
	  (cons ms (split-into-monotonics (drop (count ms) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
	(apply concat (map (fn [[fst & rst]] (map (fn [x] (cons fst x)) (permutations rst))) (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
	(let [insert (fn [set elem] [set (conj set elem)])]
	  (apply concat (map (fn [set] (insert set (first a-set))) (powerset (rest a-set)))))))
