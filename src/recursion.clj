(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
	(first coll)
    (my-last(rest coll))))

(defn max-element [a-seq]
  (if(empty? a-seq)
    nil
    (if(singleton? a-seq)
	  (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let[check? (fn check? [s1 s2] (if(or (empty? s1) (empty? s1))
      						(if(empty? s1)
        					  true
        					  false)
      						(check? (rest s1)(rest s2))))]

	(if(check? seq-1 seq-2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)(longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if(empty? a-seq)
	a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
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
	  ()))

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
    (and (empty? a-seq) (empty? b-seq))
   	  true
    (= (first a-seq)(first b-seq))
      (seq= (rest a-seq)(rest b-seq))
    :else
   	  false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
	  ()
    :else
      (cons(f (first seq-1)(first seq-2)) (my-map f (rest seq-1)(rest seq-2)))))

(defn power [n k]
  (cond
   	(== 0 k)
   	  1
    (== 1 k)
      n
    :else
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   	(or (== 0 n) (== 1 n))
   	  n
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   	(<= how-many-times 0)
	  '()
    :else
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
   	(<= up-to 0)
	  '()
    :else
      (cons (- up-to 1)(my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      ['()]
    :else
   (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
 (map reverse (tails (reverse a-seq))))

(defn rotate [a-seq] 
  (if (empty? a-seq)
    '()
    (concat (rest a-seq) (cons (first a-seq) '()))))

(defn rotations [a-seq]
 (let [rotations-helper (fn go-trough [x]
                          (if (= a-seq x)
                            (cons x '())
                            (concat (cons x '()) (go-trough (rotate x)))))]
   (rotations-helper (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) 
      freqs
    (contains? freqs (first a-seq)) 
      (my-frequencies-helper (assoc-in freqs [(first a-seq)] (inc (get-in freqs [(first a-seq)]))) (rest a-seq))
    :else 
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elem (first a-map)
        first-key (first elem)
        first-val (second elem)]
    (if(empty? a-map) 
      []
      (concat (repeat first-val first-key) (un-frequencies (rest a-map))))))

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