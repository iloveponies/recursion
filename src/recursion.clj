(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
   (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [a (first a-seq) b (first (rest a-seq))]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (if (= a (max a b)) (max-element (cons a (rest (rest a-seq)))) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [a (first a-seq) b (first (rest a-seq))]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (if (= a (seq-max a b)) (longest-sequence (cons a (rest (rest a-seq)))) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
	  false
	(= elem (first a-seq))
	  true
	:else
	  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
       a-seq
	(pred? (first a-seq))
  	  (cons (first a-seq) (my-take-while pred? (rest a-seq)))
	:else
	   []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
       a-seq
	(pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
	:else
	   a-seq))

(defn seq= [a-seq b-seq]
  (cond
     (or (and (empty? a-seq) (not (empty? b-seq)))
         (and (not (empty? a-seq)) (empty? b-seq)))
        false
     (and (empty? a-seq) (empty? b-seq))
       true
     (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
     :else
        false))

(defn my-map [f seq-1 seq-2]
  (cond
     (or (empty? seq-1) (empty? seq-2))
	    ()
	 :else
	 (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
     1
     (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0)
     0
    (= n 1)
     1
    :else
	  (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
     (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
	 ()))

(defn my-range [up-to]
  (if (> up-to 0)
     (cons (dec up-to) (my-range (dec up-to)))
	 ()))

(defn tails [a-seq]
  (if (empty? a-seq)
     (cons () nil)
     (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
     (cons () nil)
     (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn myconcat [seq1 seq2]
  (if (not (empty? (first seq1)))
     (cons (concat (first seq1) (first seq2)) (myconcat (rest seq1) (rest seq2)))
     ()))

(defn rotations [a-seq]
  (let [seq1 (tails a-seq)
       seq2 (reverse (inits a-seq))]
    (if (empty? a-seq)
      (cons () ())
      (myconcat seq1 seq2))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freq (if (contains? freqs elem) (assoc freqs elem (inc (get freqs elem))) (assoc freqs elem 1))]
      (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

