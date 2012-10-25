(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? (rest coll)) 
    (if (= (first coll) nil) false true) 
    false ))

(defn my-last [coll]
  (if (empty? (rest coll)) 
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond 
   	(empty? (rest a-seq)) (first a-seq)
    (empty? a-seq) 0
    :else (max (first a-seq) (max-element (rest a-seq) ))
  ))

(defn seq-max-helper [seq-1 seq-2]
  (cond
    (empty? seq-1) 2
    (empty? seq-2) 1
  	:else (seq-max-helper (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  (if (= (seq-max-helper seq-1 seq-2) 2) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (cond 
   	(empty? (rest a-seq)) (first a-seq)
    (empty? a-seq) 0
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq) ))
  ))

(defn my-filter [pred? a-seq]
  (remove nil? (map (fn [x] (if (pred? x) x nil)) a-seq)))

(defn sequence-contains? [elem a-seq]
  (cond
    (= elem (first a-seq)) true
    (empty? a-seq) false
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (defn inner-while [pred? a-seq new-seq]
    (if (empty? a-seq) 
      new-seq
      (if (pred? (first a-seq))
        (inner-while pred? (rest a-seq) (conj new-seq (first a-seq)))
         new-seq)))
  (inner-while pred? a-seq []))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))
    	

(defn seq= [a-seq b-seq]
  (if (= (first a-seq) (first b-seq))
    (if (and (empty? a-seq) (empty? b-seq))
      true
      (seq= (rest a-seq) (rest b-seq)))
    false))

(defn my-map [f seq-1 seq-2]
  (defn my-inner-map [f seq-1 seq-2 new-seq]
    (cond 
     (empty? seq-1) new-seq
     (empty? seq-2) new-seq
     :else (my-inner-map f (rest seq-1) (rest seq-2) 
                    (conj new-seq (f (first seq-1) (first seq-2))))))
  (my-inner-map f seq-1 seq-2 []))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
	n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (defn my-inner-repeat [how-many-times what-to-repeat new-seq]

    (if (> how-many-times 0)
      (my-inner-repeat (- how-many-times 1) what-to-repeat (conj new-seq what-to-repeat))
      new-seq))
  (my-inner-repeat how-many-times what-to-repeat []))

(defn my-range [up-to]
  (defn my-inner-range [new-seq x]
    (if (>= x 0)
      (my-inner-range (conj new-seq x) (- x 1))
      new-seq))
  (my-inner-range [] (- up-to 1)))

(defn tails [a-seq]
  (defn inner-tails [a-seq new-seq]
    (if (empty? a-seq)
      new-seq
      (inner-tails (rest a-seq) (conj new-seq a-seq))))
  (conj (inner-tails a-seq []) []))

(defn inits [a-seq]
  (defn inner-inits [a-seq new-seq]
    (if (empty? a-seq)
      new-seq
      (inner-inits (rest a-seq) (conj new-seq (reverse a-seq)))))
  (conj (inner-inits (reverse a-seq) []) []))

(defn rotations [a-seq]
  (defn rotations-inner[a-seq original all]
    (let [se (concat (rest a-seq) [(first a-seq)])]
      (if (= se original) 
        (conj all original)
        (rotations-inner se original (conj all se)))))
  (if (empty? a-seq) 
    [[]]
    (rotations-inner a-seq a-seq [])
    ))

(defn my-frequencies-helper [freqs a-seq]
  (let [oldval
        (if (contains? freqs (first a-seq)) 
          (freqs (first a-seq)) 
          0)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs (first a-seq) (inc oldval)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [helper (fn [a-map ret]
                 (let [[x y] (first a-map)]
                   (if (empty? a-map)
                     ret
                     (recur (rest a-map) (concat ret (repeat y x))))))]
   (helper a-map [])))

(defn my-take [n coll]
  (defn my-inner-take [n coll ret]
    (if (and (> n 0) (not (empty? coll)))
      (my-inner-take (- n 1) (rest coll) (conj ret (first coll)))
      ret))
  (my-inner-take n coll []))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (- n 1) (rest coll))
    coll))

(defn halve [a-seq]
  (let [mid (Math/floor (/ (count a-seq) 2))
        a (my-take mid a-seq)
        b (my-drop mid a-seq)]
    (conj (conj [] a) b)))

(defn seq-merge [a-seq b-seq]
  (defn inner-merge [a-seq b-seq order]
    (cond
      (empty? a-seq) (concat order b-seq)
      (empty? b-seq) (concat order a-seq)
      :else (if (< (first a-seq) (first b-seq))
              (inner-merge (rest a-seq) b-seq (concat order [(first a-seq)]))
              (inner-merge a-seq (rest b-seq) (concat order [(first b-seq)])))))
  (inner-merge a-seq b-seq []))

(defn merge-sort [a-seq]
  (let [[a b] (halve a-seq)]
    (cond 
     (> (count a) 1) (seq-merge (merge-sort a) (merge-sort b))
     :else (seq-merge a b)
     )))

(defn split-into-monotonics [a-seq]
  (defn monotonic? [a-seq]
    (cond
      (apply <= a-seq) true
      (apply >= a-seq) true
      :else false))
  (defn inner [a-seq new-seq ret]
    (cond
      (empty? a-seq) 
        (conj ret new-seq)
      (monotonic? (conj new-seq (first a-seq)) )
        (inner (rest a-seq) (conj new-seq (first a-seq)) ret)
      :else 
        (inner a-seq [] (conj ret new-seq))))
  (inner a-seq [] [] ))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

