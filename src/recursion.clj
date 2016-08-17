(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
    )
  )

(defn singleton? [coll]
  (cond
    (empty? coll) false
    :else (empty? (rest coll))
    )
  )


(defn my-last [coll]
  (let [[f r] [(first coll) (rest coll)]]
    (if (empty? r)
      f
      (my-last r)
      ))
  )

(defn max-element [a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]] 
    (if (empty? r) 
      f 
      (max f (max-element r))
      ))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
    )
  )

(defn longest-sequence [a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]] 
    (if (empty? r) 
      f 
      (seq-max f (longest-sequence r))
      ))
  )

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))
    )
  )

(defn sequence-contains? [elem a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]]
    (cond 
      (= elem f) true
      (empty? r) false
      :else (sequence-contains? elem r)
      )
    ))

(defn my-take-while [pred? a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]]
    (cond
      (empty? r)  ()
      (pred? f)   (cons f (my-take-while pred? r))
      :else       ()
      )
    ))

(defn my-drop-while [pred? a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]]
    (cond
      (empty? r)  ()
      (pred? f) (my-drop-while pred? r)
      :else a-seq     
      )
    ))

(defn seq= [a-seq b-seq]
  (let [ [fa ra] [(first a-seq) (rest a-seq)]
         [fb rb] [(first b-seq) (rest b-seq)] ]
    (cond
      (and (empty? ra) (empty? rb)) true
      (or (empty? ra) (empty? rb)) false
      (= fa fb) (seq= ra rb)
      :else false
      )
    ))

(defn my-map [f a-seq b-seq]
  (let [ [fa ra] [(first a-seq) (rest a-seq)]
         [fb rb] [(first b-seq) (rest b-seq)] ]
    (cond
      (or (empty? a-seq) (empty? b-seq)) ()
      (or (empty? ra) (empty? rb)) (list (f fa fb))
      :else (cons (f fa fb) (my-map f ra rb))
      )
    ))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))
    )
  )

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
    )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    )
  )

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))
    )
  )

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))
    )
  )

(defn inits [a-seq]
  (sort-by count (mapv reverse (tails (reverse a-seq))))
  )

(defn do-rots [a-seq n]
  (let [[f r] [(first a-seq) (rest a-seq)]
        [newrot] [(concat r (list f))]
        ]
    (if (= n 0)
      ()
      (cons newrot (do-rots newrot (dec n)))
      )
    ))

(defn rotations [a-seq]
  (if (= (count a-seq) 0)
    '(()) 
    (do-rots a-seq (count a-seq))
    )
  )

(defn my-frequencies-helper [freqs a-seq]
  (let [[f r] [(first a-seq) (rest a-seq)]]
    (if (empty? r)
      (assoc freqs f (inc (get freqs f 0)))
      (my-frequencies-helper (assoc freqs f (inc (get freqs f 0))) r)
      )
    ))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)
    )
  )

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[f r] [(first a-map) (rest a-map)]]
      (concat
        (repeat (val f) (key f))
        (un-frequencies r))
      )
    )
  )

(defn my-take [n coll]
  (if
    (or
      (= n 0)
      (empty? coll)
      )
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))
    )
  )

(defn my-drop [n coll]
  (if 
    (or (= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))
    )
  )

(defn halve [a-seq]
  (let [[halfcount] [(int (/ (count a-seq) 2))]]
    (vector (my-take halfcount a-seq) (my-drop halfcount a-seq))
    )
  )

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [[fa ra] [(first a-seq) (rest a-seq)]
          [fb rb] [(first b-seq) (rest b-seq)] ]
            (if (<= fa fb)
              (cons fa (seq-merge ra b-seq))
              (cons fb (seq-merge a-seq rb))
              ))
    )
  )

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[h1 h2] (halve a-seq)]
      (seq-merge (merge-sort h1) (merge-sort h2))
      )
    )
  )

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    () 
    (let [[p] [(filter (complement empty?) (inits a-seq))] 
          [first-mon-inc] [(last (filter #(apply < %) p))] 
          [first-mon-dec] [(last (filter #(apply > %) p))] ]
      (if (> (count first-mon-inc) (count first-mon-dec)) 
        (cons first-mon-inc (split-into-monotonics (drop (count first-mon-inc) a-seq)))
        (cons first-mon-dec (split-into-monotonics (drop (count first-mon-dec) a-seq)))
        )
      ))
  )

(defn insert-nth [n item a-seq]
  (concat (take n a-seq) (list item) (drop n a-seq))
  )

(defn add-in-all-spots [item a-seq]
  (map #(insert-nth % item a-seq) (range (inc (count a-seq))))
  )

(defn permutations [a-set]
  (cond
    (empty? a-set) #{()}
    (<= (count a-set) 1) a-set
    (=  (count a-set) 2) (list (apply list a-set) (reverse a-set))
    :else (apply concat (map #(add-in-all-spots (first a-set) %) (permutations (rest a-set))))
    )
  )

(defn add-to-each [item a-seq] 
  (if (empty? a-seq) 
    (cons item a-seq) 
    (map #(cons item %) a-seq)
  )
  )

(defn powerset [a-set]
  (if
    (empty? a-set) #{} 
    (let 
      [
       [f r] [(first a-set) (rest a-set)]
       [powr] [(powerset r)]
       ]
     (if (empty? r)
      (list (list f) ()) 
      (concat powr (add-to-each f powr))
      )
    )
  ))

