(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (= (count coll) 1)
    true
    false))

(defn my-last [coll]
  (if (= (count (rest coll)) 0)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (= (count a-seq) 1)  
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))
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
    (if (= (count a-seq) 1)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))
      )
    )
  )

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))
    )
  )

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem ) true
    :else (sequence-contains? elem (rest a-seq)))
  )

(defn my-take-while [pred? a-seq]
  (cond 
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else (my-take-while pred? '())                            
    )
  )

(defn my-drop-while [pred? a-seq]                      
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
    )
  )

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (and (empty? a-seq) (not (empty? b-seq))) 
        (and (not (empty? a-seq)) (empty? b-seq))) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    )
  )

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
  )

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k))))
  )

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)) ) 
    ) 
  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    (= how-many-times 1) (conj '() what-to-repeat)
    :else (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)
    ) 
  )

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (conj (my-range (dec up-to)) (dec up-to)))
  )

(defn tails [a-seq]
  (if (empty? a-seq) 
    ['()]
    (conj (tails (rest a-seq)) a-seq))
  )

(defn inits [a-seq]
  (if (empty? a-seq) 
    ['()]
    (conj (inits (drop-last 1 a-seq)) a-seq))
  )

(defn rotations [a-seq]
  (let [length (count a-seq)
        helper (fn [x]
                 (take length (drop x (concat a-seq a-seq))))]
    (if (= 0 (count a-seq))
           (list '())
            (map helper (range length))))
  )

(defn my-frequencies-helper [freqs a-seq]
  (let [current (first a-seq)
        next-seq (rest a-seq)
        current-count (get freqs current 0)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs current (inc current-count)) next-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))
 
(defn un-frequencies [a-map]
  (let [element (first a-map)]
    (if (not (empty? element)) 
      (let [times (second element)
            el (first element)
            next-int (rest a-map)]
        (concat (repeat times el) (un-frequencies next-int))
        ))
    )
  )

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

