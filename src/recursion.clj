(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (= false (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (> (count coll) 1)
    (my-last (rest coll))
    (first coll)))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (== (count a-seq) 1) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
   (> (count seq-1) (count seq-2)) seq-1
   :else seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (== (count a-seq ) 1) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [apu
        (fn [a-seq p-seq]
          (cond
          (empty? a-seq) p-seq
		  (pred? (first a-seq)) (recur (rest a-seq) (conj p-seq (first a-seq)))
		  :else (recur (rest a-seq) p-seq)
           )
          )]
    (apu a-seq [])))

(defn sequence-contains? [elem a-seq]
  (cond
  (= (first a-seq) nil) false
  (= (first a-seq) elem) true
  :else (recur elem (rest a-seq))
  ))

(defn my-take-while [pred? a-seq]
  (let [apu
        (fn [a-seq p-seq]
          (cond
          (empty? a-seq) p-seq
		  (pred? (first a-seq)) (recur (rest a-seq) (conj p-seq (first a-seq)))
		  :else p-seq
           )
          )]
    (apu a-seq [])))

(defn my-drop-while [pred? a-seq]
  (let [apu
        (fn [a-seq p-seq]
          (cond
          (empty? a-seq) a-seq
		  (pred? (first a-seq)) (recur (rest a-seq) p-seq)
		  :else a-seq
           )
          )]
    (apu a-seq [])))

(defn seq= [a-seq b-seq]
  (if (and (= 1 (count a-seq)) (= 1 (count b-seq))) (if (= (first a-seq) (first b-seq)) true false)
  (if (== (count a-seq) (count b-seq)) 
	(if (== (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
      false)
    false)
    ))

(defn my-map [f seq-1 seq-2]
  (let [apu
        (fn [p-seq seq-1 seq-2]
          (cond
          (or (empty? seq-1) (empty? seq-2)) p-seq
          :else (recur (conj p-seq (f (first seq-1) (first seq-2))) (rest seq-1) (rest seq-2))))]
    (apu [] seq-1 seq-2)))

(defn power [n k]
  (reduce * (repeat k n)))

(defn fib [n]
  (letfn [(fib
         [nykyinen seuraava n]
         (if (== 0 n)
           nykyinen
           (fib seuraava (+ nykyinen seuraava) (dec n))))]
    (fib 0 1 n)))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
  (>= 0 how-many-times) ()
  :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
  (= 0 up-to) ()
  :else (cons (dec up-to) (my-range (dec up-to)))
  ))

(defn tails [a-seq]
  (cond
  (empty? a-seq) (cons a-seq ())
  :else (cons a-seq (tails (rest a-seq)))
   ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [apu
     (fn [n b-seq]
       (cond
       (> 0 n) b-seq
       :else (recur (dec n) (conj (rest b-seq) (first b-seq)))))]
    (apu (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

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